#' Download Sentinel-2 data (L1A) from the public GoogleCloud Bucket
#'
#' @param aoi An \code{sf} or \code{Spatial} object with the extent for which to
#'   download S2 data. Note that only its bounding box is going to be used to
#'   determine the final extent.
#' @param rootdir A character vector of length 1 indicating and existing directory
#'   either relative or absolute. Based on the value in \code{extent_name} a
#'   subdirectory will be created within \code{rootdir} where the data
#'   will be written to. Default is to the current directory ".".
#' @param extent_name A character vector of length 1 specifying the name of the
#'   subdirectory within rootdir where the data will be downloaded. This variable
#'   makes it easy to distinguish between different projects.
#' @param max_cloud A integer vector of length 1 indicating the percentage of cloud
#'   cover in the scenes which will be considered for download. Any scene with a
#'   cloud cover higher than this value will not be downloaded.
#' @param time_window A length two character specifying the time interval to be
#'   downloaded in the form of \code{YYYY-MM-DD}. In cases where the years of the
#'   start year and end year differ and he option \code{time_period = "seasonal"},
#'   data will be downloaded for the specified season for all years in the interval.
#'   Otherwise, when \code{time_period = "all"}, the complete interval will be downloaded.
#' @param time_period A length one character specifying the download behavior in
#'   terms of the time_window variable. In cases where \code{time_period = "seasonal"}
#'   the data will be downloaded seasonally between the start year and end year.
#'   When \code{time_period = "full"} the complete interval will be downloaded.
#' @param use_db A logical parameter indicating if the tile index
#'   should be saved locally as an SQLite database. This significantly
#'   speeds up future queries but also increases the local disk
#'   space used (from about 2 GB to 5 GB). The default value is \code{TRUE}.
#' @param force_update A logical parameter indicating if the
#'   latest version of the tile index should be downloaded from
#'   the Google Cloud. The default value is \code{FALSE}.
#' @param query_only A logical parameter indication if the data should be downloaded
#'   or if the query only should be processed. The default value is \code{FALSE},
#'   in case it is \code{TRUE} the function will return a dataframe with
#'   information about the matching scenes.
#' @param create_cloudmask A logical indicating if a cloud mask should be created
#'   during download. Defaults to \code{TRUE}.
#'
#' @note For downloading from the public Sentinel-2 data bucket \code{gsutil} is used.
#'   This needs to be installed in advance and the path to the binary should be made
#'   available in the environment variable. Additionally, before downloading
#'   you have to log in with a Google account using \code{gcloud init} in the command
#'   line. Users are advised to use the provided Dockerfile to create an image and
#'   run \code{sen2tool} in a container. This Dockerfile automatically installs
#'   all required external dependencies such as \code{gsutils} and \code{gdal} binaries.
#'
#' @return A character vector of variable length corresponding to the original \code{SAFE} name
#'   of successfully downloaded data which fall into the scope of the query specified
#'   by extent and timewindow and possibly also footprint check.
#' @author Darius Görgen, Dr. Fabian Löw (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
#'
#' @export downloadS2
#' @importFrom sf st_as_sf st_as_sfc st_bbox st_union st_coordinates st_transform
#' @importFrom stringr str_sub
#' @importFrom R.utils gunzip
#' @importFrom utils download.file
#' @importFrom sqldf read.csv.sql
#' @importFrom dplyr summarise
downloadS2 <- function(aoi = NULL,
                       time_window = c("2016-06-01", "2019-09-30"),
                       rootdir = ".",
                       extent_name = "projectname",
                       max_cloud = 100,
                       time_period = "seasonal",
                       use_db = TRUE,
                       force_update = FALSE,
                       query_only = FALSE,
                       create_cloudmask = TRUE){

  # cast to sf when class is not sf (e.g. sfc or Spatial)
  if(class(aoi)[1] != "sf"){
    tryCatch(aoi <- st_as_sf(aoi))
  }

  time_window = as.Date(time_window)

  # declare datadir based on input
  datadir = file.path(rootdir, extent_name, "L1C")

  # create a bounding box for the aoi
  aoi2 = st_transform(aoi, st_crs(4326))
  aoi2 = st_as_sfc(st_bbox(st_union(aoi2)))
  coordinates = st_coordinates(aoi2)

  n_lat = max(coordinates[,2])
  s_lat = min(coordinates[,2])
  w_lon = min(coordinates[,1])
  e_lon = max(coordinates[,1])

  # update db based on user input
  update_indexDB(use_db = use_db, force_update = force_update)

  if(use_db){
    # query file with extent and cloud cover
    message("Querying database for matching results")
    index = suppressWarnings(sqldf(paste0("select * from tiles where NORTH_LAT >= ", s_lat,
                                          " and SOUTH_LAT <= ", n_lat,
                                          " and WEST_LON <= ", e_lon,
                                          " and EAST_LON >= ", w_lon,
                                          " and CLOUD_COVER <= ", max_cloud),
                                   dbname = file.path(find.package("sen2tool"), "tilesDB")))
  } else {
    message("Querying csv file for matching results. (This could take a while...)")
    index = suppressWarnings(read.csv.sql(file = file.path(find.package("sen2tool"), "tiles.csv"),
                                          sql = paste0("select * from tiles where NORTH_LAT >= ", s_lat,
                                                       " and SOUTH_LAT <= ", n_lat,
                                                       " and WEST_LON <= ", e_lon,
                                                       " and EAST_LON >= ", w_lon,
                                                       " and CLOUD_COVER <= ", max_cloud)))
  }

  # create time_intervals
  time_intervals <- if (time_period == "seasonal") {
    data.frame(
      "start" = strftime(seq(time_window[1], time_window[2], by = "year"), "%Y-%m-%d"),
      "end" = strftime(rev(seq(time_window[2], time_window[1], by = "-1 year")), "%Y-%m-%d"),
      stringsAsFactors = FALSE
    )
  } else if (time_period == "full") {
    data.frame(
      "start" = time_window[1],
      "end" = time_window[2],
      stringsAsFactors = FALSE
    )
  }

  out_list = lapply(1:nrow(time_intervals), function(i){
    hit = index[which(index$SENSING_TIME > as.Date(time_intervals$start[i]) & index$SENSING_TIME < as.Date(time_intervals$end[i])),]
    return(hit)
  })
  out_list = do.call(rbind, out_list)
  # check which products are Level 1C
  out_list = out_list[which(startsWith(out_list$GRANULE_ID, "L1C")),]

  message(paste0("\n The query delivered ", nrow(out_list), " results for your AOI and time window."))

  if(!query_only) { # when query_only = FALSE
    message("\n Starting to download data from GoogleCloud Bucket.")
    names = basename(out_list$BASE_URL)

    # download data in for loop and skip if files are already present in datadir.
    for (i in 1:nrow(out_list)){
      print(i)
      if(file.exists(file.path(datadir, names[i]))){
        message(paste0("\n File ", names[i], " already exists in datadir. Skipping download."))
        next
      } else {

        # prepare command with gsutil
        command = paste0("gsutil -m cp -r ", out_list$BASE_URL[i], " ", datadir)
        dir.create(datadir, showWarnings = F, recursive = T)
        # try to download and save output in s
        s = try(suppressWarnings(system(command, intern = T, ignore.stderr = T)))

        # some files are not present in GoogleCloud Bucket, thus the download fails
        # this is only the case for very few files
        if(!is.null(attributes(s)$status)){
          message(paste0("\n File ", names[i], " could not be downloaded."))

        } else {
          # creating some empty folders which are needed by sen2r/sen2cor
          # functionality to allow further processing
          dir.create(file.path(datadir, names[i], "AUX_DATA"), showWarnings = F)
          dir.create(file.path(datadir, names[i], "HTML"), showWarnings = F)
          message(paste0("\n File ", names[i], " succesfully downloaded."))
          if(create_cloudmask) createCloudMask(file.path(datadir, names[i]), overwrite = F)
        }
      }
    }


    # clean name vector for successful downloads
    success = list.files(datadir) # list all files found in datadir
    names = names[names %in% success] # check which files are present in our name vector
    message(paste0("\n", length(names), " files successfully download."))
    return(names) # returning only matches which were present in the original query and downloaded successfully
  } else { # when query_only = TRUE
    return(out_list)
  }
}

#' Cloud Mask raster for Sentinel 2 L1C
#'
#' This function is used to create a \code{GTiff} cloud mask from
#' the \code{gml} vectorized cloud mask product of Sentinel Level 1C data.
#' A \code{GTiff} file called \code{<tile-id>_CM.tif}
#' is written to \code{GRANULE/<tile-id>/IMG_DATA} in 60m resolution. A value
#' of 1 indicates the presence of a cloud while a value of 0 represents
#' clear sky conditions according to the cloud product.
#'
#' @param safe A length one character vector pointing to the root
#'   of a \code{SAFE} directory.
#' @param overwrite A logical indicating if an already existing cloud
#'   mask raster should be overwritten. Defaults to \code{TRUE}
#'
#' @return Nothing. However, a \code{GTiff} file is written into the imagery
#'   directory of the \code{SAFE} product found at \code{GRANULE/<tile-id>/IMG_DATA/<tile-id>_CM.tif}.
#'
#' @export createCloudMask
#' @importFrom rgdal ogrListLayers
#' @importFrom sf st_read st_crs
#' @importFrom raster raster rasterize writeRaster
#' @importFrom stringr str_split
#' @author Darius Görgen, Dr. Fabian Löw (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
createCloudMask <- function(safe, overwrite = T){

  # prepare name vectors
  tile_name = list.files(file.path(safe, "GRANULE"))
  dummy = list.files(safe, "B10.jp2$", recursive = T, full.names = T)
  cloud_mask = list.files(safe, "MSK_CLOUDS_B00.gml$", recursive = T, full.names = T)
  pre_name = paste(str_split(basename(dummy), "_")[[1]][c(1,2)], collapse = "_")
  filename = paste0(file.path(safe, "GRANULE", tile_name, "IMG_DATA/"), pre_name, "_CM.tif")

  if(file.exists(filename) & !overwrite){
    NULL
  } else {
  # check if any clouds are present
    if(length(ogrListLayers(cloud_mask)) == 0){
      # write raster with 0 as cloud mask at 60m resolution
      dm = raster(dummy)
      dm[] = 0
      writeRaster(dm, filename,
                  overwrite = overwrite,
                  datatype = "INT1U")
    } else {
      # read in the cloudmask and rasterize it to the dummy resultion (60m)
      cm = st_read(cloud_mask, quiet = T)
      cm$cloud = 1
      dm = raster(dummy)
      dm[] = 0
      sf::st_crs(cm) = st_crs(dm)
      cm = rasterize(cm, dm, field = "cloud")
      writeRaster(cm, filename,
                  overwrite = overwrite,
                  datatype = "INT1U")
    }
  }
}



#' Calculate a spectral index with gdalcubes
#'
#' This function is used internally to calculate a spectral index using the
#' gdalcubes package. It is called by \code{calcIndex()} on a given set of \code{SAFE}
#' directories. A user defined cube view defined by the x and y resolution,
#' the time resolution, a CRS, as well as aggregation methods is applied.
#' Additionally, the time dimension can be aggregated which is useful for seasonal
#' calculations of a given index. The output of the function is a \code{GTiff} file
#' for each time step labeled as \code{<label>_<index>_<timestep>.tif}. Zonal statistics
#' are written to a Geopackage for each timestep labeled as \code{<label>_<index>_<timestep>.gpkg}
#' to outdir.
#'
#' @param safes A character vector with the \code{.jp2} and \code{.tif} files of the
#'   selected \code{SAFE} directories. Obtained with \code{list.files(<safedir>, "jp2$|.tif$", recursive =T, full.names = T)}
#' @param aoi A sf-object with POLYGONS or MULTIPOLYGONS. For objects with ZM
#'   dimensions these dimensions are dropped before calculation.
#' @param epsg A character specifying the epsg code for the the cube view specified
#'   as \code{"EPSG:<code>"}. The resulting \code{GTiff} files and the \code{GPKG}
#'   with the zonal statistics will be projected into this CRS.
#'   \code{dx} and \code{dy} are understood in the units of the CRS specified here.
#'   If left empty, the CRS of the \code{aoi} variable will be used.
#' @param dx A numeric specifying the resolution of the output raster in x dimension
#'   expressed in the map unit specified in \code{epsg.} If \code{epsg} is left
#'   empty it defaults to the map unit of \code{aoi.}
#' @param dy A numeric specifying the resolution of the output raster in y dimension
#'   expressed in the map unit specified in \code{epsg}.
#'   If \code{epsg} is left empty it defaults to the map unit of \code{aoi}.
#' @param dt Resolution of the time dimension expressed as ISO8601 string
#'   (e.g. \code{"P16D"} for 16 days).
#' @param aggregation A character vector specifying the aggregation method for
#'   images of the same time stamp. Must be one of \code{"min"}, \code{"max"},
#'   \code{"mean"}, \code{"median"}, or \code{"first"}. Defaults to \code{"mean"}.
#' @param resampling A character specifying the resampling method used to transform
#'   images from their native CRS to the output CRS. Can be any of the methods
#'   supported by gdalwarp (see \url{www.gdal.org/programs/gdalwarp.html}).
#'   Defaults to \code{"bilinear"}.
#' @param tmpagg A logical indicating if temporal aggregation of all timestemps should
#'   be performed calling \code{\link[gdalcubes:reduce_time]{reduce_time()}}. Defaults to \code{FALSE}.
#' @param index A character specifying the short name of the index to be calculated.
#'   Available indices can be checked with \code{check_indices()}.
#' @param label A character label which is appended to the output files in the
#'   form of \code{<label>_<index>_<timestep>}. Files with the same name in
#'   \code{outdir} will be overwritten without warning.
#' @param outdir A character specifying an existing directory where output files
#'   will be written to. Defaults to \code{"."}.
#' @param zonalstat A character specifying the aggregation statistic to be used by
#'   \code{\link[gdalcubes:zonal_statistics]{zonal_statistics()}}.
#'   Multiple selections are supported. Must be included in \code{"min"}, \code{"max"},
#'   \code{"mean"}, \code{"median"},
#'   \code{"count"}, \code{"sum"}, \code{"prod"}, \code{"var"}, and \code{"sd"}.
#'   Defaults to \code{"mean"}.
#' @param threads Number of cores used for parallel processing
#'
#'
#' @return Nothing. However a \code{GTiff} and a \code{GPKG} with the zonal statistics are written to
#'   \code{outdir} for every resulting timestep.
#' @note See \code{\link[gdalcubes:cube_view]{cube_view()}} and \code{\link[gdalcubes:zonal_statistics]{zonal_statistics()}} for more information.
#' @export cube_indexcalc
#' @keywords internal
#' @importFrom gdalcubes create_image_collection extent cube_view raster_cube select_bands apply_pixel reduce_time write_tif zonal_statistics image_mask gdalcubes_options
#' @importFrom sf st_crs st_transform st_zm st_bbox
#' @importFrom stringr str_extract_all str_remove
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @author Darius Görgen, Dr. Fabian Löw (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
cube_indexcalc <- function(safes,
                           aoi,
                           epsg = NULL,
                           dx = NULL,
                           dy = NULL,
                           dt = NULL,
                           aggregation = "mean",
                           resampling = "bilinear",
                           tmpagg=FALSE,
                           index = NULL,
                           label = "",
                           outdir = ".",
                           threads){

  . = NULL # for devtools::check() not throwing errors
  indices = read.csv(system.file("indices_sen2tool.csv", package = "sen2tool"))
  if (!index %in% indices$name){
    stop("Specified index cannot be calculated. Chose another one.")
  }

  if (!aggregation %in% c("min", "max", "mean", "median", "first")){
    stop(paste0('Selected aggregation method for images of the same timestamp not supported\n',
                'Choose one of "min", "max", "mean", "median", or "first"'))
  }

  gdalcubes_options(threads = threads)
  s2.col = create_image_collection(safes, system.file("sentinel2_l1c_w_cm.json", package = "sen2tool"))
  extent = extent(s2.col)

  if (is.null(epsg)){
    warning("No epsg variable was specified. Now using the CRS of the aoi variable. dx and dy are interpreted as map units.")
    epsg = paste0("EPSG:",as.character(st_crs(aoi)$epsg))
  }

  aoi = st_transform(aoi, st_crs(epsg))
  aoi = st_zm(aoi, drop = T)
  extent$left = st_bbox(aoi)[1]
  extent$right = st_bbox(aoi)[3]
  extent$top = st_bbox(aoi)[4]
  extent$bottom = st_bbox(aoi)[2]

  cubeview = cube_view(extent = extent,
                       srs = epsg,
                       dx = dx,
                       dy = dy,
                       dt = dt,
                       aggregation = aggregation,
                       resampling = resampling)

  indices$name = as.character(indices$name)
  i = which(indices$name == index)
  term = paste0("(", indices$s2_formula[i], ")")
  bands = unique(str_extract_all(term, "band_([0-9]|1[0:2]|8a)")[[1]])
  # initiate cube and calculate index
  raster_cube(s2.col, cubeview, mask = image_mask("CM", values = 1)) %>%
    select_bands(bands) %>%
    apply_pixel(term, names = indices$name[i]) %>%
    {if(tmpagg) reduce_time(., paste0("mean(", indices$name[i], ")")) else . } %>%
    write_tif(dir = outdir, prefix = paste0(label,"_",indices$name[i], "_")) -> rasterfiles
  rm(s2.col, cubeview); gc()
  return(rasterfiles)
}


#' Calculate Zonal statistics with gdlcubes
#'
#' @param idcol A column name of the \code{aoi} object uniquely identifying the polygons.
#'   The values found in this columns will be appended to the resulting sf object
#'   to allow feature identification.
#' @param rasterfiles Input raster files
#' @param dates Input dates
#' @param band_name Input band names
#' @param zonalstat Zonal stats to calculate
#'
#' @return A \code{sf} object in long format with columns for the specified zonal statistics,
#'  and rows for each feature and input time.
#' @inheritParams cube_indexcalc
#' @export calcZS
#' @importFrom gdalcubes gdalcubes_options create_image_collection extent cube_view raster_cube zonal_statistics
#' @importFrom sf st_transform st_crs st_zm st_bbox st_as_sf
#' @importFrom magrittr %>%
#' @author Darius Görgen, Dr. Fabian Löw (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
calcZS <- function(aoi, idcol, rasterfiles, dates, band_name, zonalstat,
                   epsg, dx, dy, dt, aggregation, resampling, threads){

  if(zonalstat == "all") zonalstat =  c("min", "max", "mean", "median", "count", "sum", "prod", "var","sd")

  if(!all(zonalstat %in% c("min", "max", "mean", "median", "count", "sum", "prod", "var","sd"))){
    stop(paste0('Selected aggregation method for zonal statistics not supported\n',
                'Choose one of "min", "max", "mean", "median", "count", "sum", "prod", "var","sd"'))
  }

  if (!requireNamespace("stars", quietly = TRUE))
    stop("stars package is required but not installed. Please install it first.")

  # initiate new collection
  gdalcubes_options(threads = threads)
  index.col = create_image_collection(rasterfiles, date_time = dates, band_names = band_name)
  extent = extent(index.col)
  aoi = st_transform(aoi, st_crs(epsg))
  aoi = st_zm(aoi, drop = T)
  extent$left = st_bbox(aoi)[1]
  extent$right = st_bbox(aoi)[3]
  extent$top = st_bbox(aoi)[4]
  extent$bottom = st_bbox(aoi)[2]

  cubeview = cube_view(extent = extent,
                       srs = epsg,
                       dx = dx,
                       dy = dy,
                       dt = dt,
                       aggregation = aggregation,
                       resampling = resampling)

  term = paste(zonalstat, paste0("(", band_name, ")"), sep = "")
  raster_cube(index.col, cubeview) %>%
    zonal_statistics(aoi, expr = term, as_stars = TRUE, overwrite = TRUE) -> stats
  stats = st_as_sf(stats, long = T)
  stats$id = rep(1:nrow(aoi), length(unique(stats$time)))
  return(stats)
}




#' Spectral index and zonal statistics
#'
#' This function is called by users to calculate a spectral index and extract zonal
#' statistics for a sf object containing \code{POLYGONS} or \code{MULTIPOLYGONS}. Any \code{ZM} dimensions
#' of the \code{sf} object are dropped before calculation. Users can specify the timeframe
#' of the calculation as \code{"seasonal"}. In this way, the calculation for each year
#' specified in the \code{years} variables is done separately. This allows users to only
#' cover specific timeframes within several years. If timeframe is \code{"complete"}
#' it is assumed that the complete timeseries of the input files should be covered
#' by the temporal resolution specified in the \code{dt} variable.
#'
#' @param timeframe A character of either \code{"seasonal"} or \code{"complete"} specifying
#'   if the calculation should be applied for the complete time series or for
#'   each year independently. Defaults to \code{"complete"}. If \code{"seasonal"} is specified
#'   the \code{years} variables cannot be left empty.
#' @param years A character vector specifying the individual years for the calculation
#'   in case timeframe is \code{"seasonal"} in the form of e.g. \code{c("2016", "2017")}.
#'
#' @return Nothing. However a \code{GTiff} and a \code{GPKG} with the zonal statistics are written to
#'   \code{outdir} for every resulting timestep.
#' @note See \code{\link[gdalcubes:cube_view]{cube_view()}} and \code{\link[gdalcubes:zonal_statistics]{zonal_statistics()}} for more information.
#' @export calcIndex
#' @inheritParams cube_indexcalc
#'
#' @importFrom stringr str_sub
#' @author Darius Görgen, Dr. Fabian Löw (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
calcIndex <-  function(safes,
                       aoi,
                       epsg = NULL,
                       dx = NULL,
                       dy = NULL,
                       dt = NULL,
                       aggregation = "mean",
                       resampling = "bilinear",
                       tmpagg = FALSE,
                       index = NULL,
                       label = "",
                       outdir = ".",
                       timeframe = "complete",
                       years = NULL,
                       threads = 1) {

  if(!timeframe %in% c("seasonal", "complete")){
    stop("Variable timeframe need to be one of seasonal or complete!")
  }

  if(any(c(is.null(dx), is.null(dy), is.null(dt), is.null(index)))){
    stop("Either one of dx, dy, dt, or index is NULL. Cannot be NULL.")
  }

  if (timeframe == "seasonal" & is.null(years)){
    stop("Cannot calculate a seasonal timeframe without years specified for which to calcluate the index idependently!")
  }

  if(timeframe == "seasonal"){
    y_index = str_sub(basename(safes), 8, 11)

    for (year in years){
      y_safes = safes[grep(year, y_index)]
      new_label = paste0(label,year)
      cube_indexcalc(y_safes, aoi, epsg, dx, dy, dt, aggregation, resampling, tmpagg, index, new_label, outdir, threads)
    }

  } else {
    cube_indexcalc(safes, aoi, epsg, dx, dy, dt, aggregation, resampling, tmpagg, index, label, outdir, threads)
  }
}


#' Get available indices
#'
#' This functions returns a dataframe with the available indices for calcualtion.
#' Use the values found in the column \code{name} to specify an individual index
#'
#' @return A dataframe
#' @export check_indices
#' @importFrom utils read.csv
#'
#' @examples
#' library(mapme.vegetation)
#' check_indices()[1:10,]
#' @author Darius Görgen, Dr. Fabian Löw (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
check_indices <- function(){
  indices = read.csv(system.file("indices_sen2tool.csv", package = "mapme.vegetation"))
  return(indices[ ,c(2:5)])
}
