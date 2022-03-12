#' Calculate Zonal statistics with gdalcubes
#'
#' @param aoi A sf object with polygons / multipolygons
#' @param idcol A column name of the \code{aoi} object uniquely identifying the polygons.
#'   The values found in this columns will be appended to the resulting sf object
#'   to allow feature identification.
#' @param files Input raster files
#' @param times Input dates
#' @param bands Input band names
#' @param zonalfuns A named list with functions for zonal statistics as elements
#' @param bbox Bounding Box
#' @param after Date after
#' @param before Date before
#' @param srs target spatial reference system as a string; can be a proj4 definition, WKT, or in the form "EPSG:XXXX"
#' @param dx resolution x
#' @param dy resolution y
#' @param dt resoltion time
#' @param aggregation aggregation method as string, defining how to deal with pixels containing data from multiple images, can be "min", "max", "mean", "median", or "first"
#' @param resampling method used in gdalwarp when images are read, can be "near", "bilinear", "bicubic" or others as supported by gdalwarp (see https://gdal.org/programs/gdalwarp.html)
#' @param outpath Path for the output GeoPakackage
#' @param threads Number of threads
#' @param ... Additional arguments to st_write
#' @return A \code{sf} object in long format with columns for the specified zonal statistics,
#'  and rows for each feature and input time.
#' @export extract_zonalstats
#' @importFrom gdalcubes gdalcubes_options create_image_collection extent cube_view raster_cube extract_geom
#' @importFrom sf st_transform st_crs st_zm st_bbox st_as_sf st_drop_geometry st_write
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
extract_zonalstats <- function(aoi = NULL,
                               idcol = NULL,
                               files = NULL,
                               times,
                               bands,
                               zonalfuns,
                               bbox,
                               after,
                               before,
                               srs,
                               dx,
                               dy,
                               dt,
                               aggregation,
                               resampling,
                               outpath = NULL,
                               threads = 1,
                               ...){

  .check_times(times)

  if (!requireNamespace("stars", quietly = TRUE))
    stop("stars package is required but not installed. Please install it first.")


  # initiate new collection
  gdalcubes_options(parallel = threads)
  col = create_image_collection(files, date_time = times, band_names = bands)
  extent = extent(col)
  if(!identical(st_crs(aoi), st_crs(srs))){
    aoi = st_transform(aoi, st_crs(srs))
  }
  ext = .update_ext(col, bbox, after = after, before = before)

  cubeview = cube_view(extent = ext,
                       srs = srs,
                       dx = dx,
                       dy = dy,
                       dt = dt,
                       aggregation = aggregation,
                       resampling = resampling)

  stat_names = names(zonalfuns)
  stats = lapply(1:length(zonalfuns), function(i){
    out = raster_cube(col, cubeview) %>%
      extract_geom(sf = aoi, FUN = zonalfuns[[i]])
    out$stat = stat_names[i]
    out
  })
  stats = do.call(rbind, stats)
  stats[idcol] = rep(unlist(st_drop_geometry(aoi)[idcol]), times = length(unique(stats$time)))
  stats = st_as_sf(left_join(stats, aoi, by = idcol))
  stats$FID = NULL

  if(!is.null(outpath)){
    st_write(stats, dsn = outpath, ...)
  }

  return(stats)
}

#' Extract pixel values of polygons from raster files
#'
#' This function extracts all pixel values within polygons based on a time series
#' of rasters. Users are expected to specify the name vector of bands as well
#' as a vector indicating the date for each raster.
#'
#' @param files A chachter vector indicating the raster files.
#' @param bands A charachter vector indicating the names of bands for each raster files.
#' @param times A charachter vector indicating the date for each raster file
#' @param aoi  An sf object of polygons for which to extract the pixel values
#'
#' @return A dataframe object with the extracted value amended by the metadata
#'   of the aoi object.
#' @export extract_pixels
#' @importFrom terra rast vect extract
#' @importFrom dplyr left_join
#' @importFrom sf st_drop_geometry
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
extract_pixels <- function(
  files = NULL,
  bands = NULL,
  times = NULL,
  aoi = NULL){

  if(!all(file.exists(files))){
    stop("Some files do not exist. Check the files argument.")
  }

  .check_times(times)

  names_vec = expand.grid(bands, times)
  names_vec = paste(names_vec$Var1, names_vec$Var2, sep = "-")
  ras_stack = rast(files)
  names(ras_stack) = names_vec
  spat_vector = vect(aoi)
  vals = as.data.frame(extract(ras_stack, spat_vector))
  aoi_meta = st_drop_geometry(aoi)
  aoi_meta$.mapmeid = 1:nrow(aoi)
  vals = left_join(vals, aoi_meta, by = c("ID" = ".mapmeid"))
  # vals$ID = NULL
  vals
}

#' Function to compare two epochs
#'
#' This function is used to compare the raster files of two different epochs.
#' The order as well as the arithmetic operation of the comparision can be
#' specified by the user. The paths to the raster files comprising the
#' first and second epoch are the main inputs. When more than one raster file
#' per epoch is handed to the function an aggregation function is applied per
#' pixel for each of the epochs.
#'
#' @param epoch1_files A character vector pointing to one or more raster files
#'   to be considered as part of the first epoch.
#' @param epoch2_files  character vector pointing to one or more raster files
#'   to be considered as part of the second epoch.
#' @param order A numeric vector of length 2 indicating the order of the epoch
#'   comparison. Either \code{c(1,2)} which will put the first epoch first or
#'   \code{c(2,1)} which will put the second epoch first.
#' @param aggregation A character vector indicating the aggregation operations
#'   applied to each pixel individually for each epoch when more than one file
#'   per epoch are handed to the function. Must be one of mean, max, min, median,
#'   sum, range, and prod.
#' @param stat A charachter vector indicating the arithmetic operation for the
#'   comparison. Must be one of "-", "+", "*", "/".
#'
#' @return A spatRaster.
#' @export compare_epochs
#' @importFrom terra rast nlyr app
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
compare_epochs <- function(
  epoch1_files = NULL,
  epoch2_files = NULL,
  order = c(1,2),
  aggregation = "mean",
  stat = "-"){

  if(!stat %in% c("-", "+", "*", "/")){
    stop("Wrong comparison operator. Must be one of -, +, *, / as a string.")
  }

  if(!aggregation %in% c("mean", "max", "min", "median", "sum", "range", "prod")){
    stop("Wrong aggregation opertator. Must be one of mean, max, min, median, sum, range, prod as a string.")
  }

  # read as spatRast
  if(is.character(epoch1_files) & is.character(epoch2_files)){
    epoch1 = rast(epoch1_files)
    epoch2 = rast(epoch2_files)
  } else if(class(epoch1_files) == "SpatRaster" & class(epoch2_files) == "SpatRaster"){
    epoch1 = epoch1_files
    epoch2 = epoch2_files
  } else {
    stop("Either provide charachter strings to raster files or two SpatRaster objects.")
  }
  # apply aggregation method
  if(nlyr(epoch1) > 1 & nlyr(epoch1) > 1){
    epoch1 = app(epoch1, aggregation)
    epoch2 = app(epoch2, aggregation)
  }
  # apply right order
  if(identical(order, c(1,2))) epochs = list(obj1 = epoch1, obj2 = epoch2)
  if(identical(order, c(2,1))) epochs = list(obj1 = epoch2, obj2 = epoch1)

  # do comparison

  if(stat %in% c("-")){
    result = epochs$obj1 - epochs$obj2
  }

  if(stat %in% c("+")){
    result = epochs$obj1 + epochs$obj2
  }

  if(stat %in% c("*")){
    result = epochs$obj1 * epochs$obj2
  }

  if(stat %in% c("/")){
    result = epochs$obj1 / epochs$obj2
  }
  result
}





