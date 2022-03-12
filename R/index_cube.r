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
#'
#' @return Nothing. However a \code{GTiff} and a \code{GPKG} with the zonal statistics are written to
#'   \code{outdir} for every resulting timestep.
#' @note See \code{\link[gdalcubes]{cube_view}} for more information.
#' @importFrom  stringr str_sub
#' @export index_cube
#' @inheritParams .calc_index
#'
#' @importFrom stringr str_sub
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
index_cube <-  function(
  files = NULL,
  format = system.file("sentinel2_l2a_cog.json", package = "mapme.vegetation"),
  DB_path = NULL,
  dx = NULL,
  dy = NULL,
  dt = NULL,
  srs = NULL,
  bbox = NULL,
  after = NULL,
  before = NULL,
  timeframe = "full",
  aggregation = "median",
  resampling = "bilinear",
  tmpagg = FALSE,
  index = NULL,
  bands = NULL,
  label = "",
  outdir = ".",
  overwrite = F,
  mask_layer = NULL,
  mask_values = NULL,
  mask_invert = FALSE,
  chunking = c(1, 256, 256),
  threads = 1,
  ...) {

  if(!timeframe %in% c("seasonal", "full")){
    stop("Variable timeframe need to be one of seasonal or complete!")
  }

  if(any(c(is.null(dx), is.null(dy), is.null(dt)))){
    stop("Either one of dx, dy, dt is NULL. Cannot be NULL.")
  }

  if(!is.null(index)){
    if(!is.null(bands)){
      stop("One of index or bands must be set to NULL.")
    }
  }

  index = sort(index)

  if(timeframe == "seasonal"){
    years = as.numeric(str_sub(c(after, before), 1, 4))
    years = as.character(seq(years[1], years[2]))
    dates = str_sub(c(after, before), 5, -1)

    out = lapply(years, function(year) {
      y_after = sprintf("%s%s", year, dates[1])
      y_before = sprintf("%s%s", year, dates[2])

      .calc_index(files = files,
                  format = format,
                  DB_path = DB_path,
                  bbox = bbox,
                  srs = srs,
                  dx = dx,
                  dy = dy,
                  dt = dt,
                  after = y_after,
                  before = y_before,
                  aggregation = aggregation,
                  resampling = resampling,
                  tmpagg = tmpagg,
                  index = index,
                  bands = bands,
                  label = label,
                  outdir = outdir,
                  overwrite = overwrite,
                  mask_layer = mask_layer,
                  mask_values = mask_values,
                  mask_invert = mask_invert,
                  chunking = chunking,
                  threads = threads,
                  ...)
    })
    names(out) = years

  } else {
    out = .calc_index(files = files,
                      format = format,
                      DB_path = DB_path,
                      bbox = bbox,
                      srs = srs,
                      dx = dx,
                      dy = dy,
                      dt = dt,
                      after = after,
                      before = before,
                      aggregation = aggregation,
                      resampling = resampling,
                      tmpagg = tmpagg,
                      index = index,
                      bands = bands,
                      label = label,
                      outdir = outdir,
                      overwrite = overwrite,
                      mask_layer = mask_layer,
                      mask_values = mask_values,
                      mask_invert = mask_invert,
                      chunking = chunking,
                      threads = threads,
                      ...)
  }
  out
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
#' @param files A character vector with the filepaths to the raster files for which
#'   gap-filling and a smoothing function shall be applied.
#' @param format A charachter vector pointing to a json file with a gdalcubes format
#'   description of the input files.
#' @param DB_path A charachter vector where the gdalcubes data base file indexing
#'   the spatiotemporal extents of files. Defaults to NULL which means a temporary
#'   file is used. Can be pointed to a file ending in ".db" to speed up later iterations.
#' @param dx A numeric specifying the resolution of the output raster in x dimension
#'   expressed in the map unit specified in \code{epsg.} If \code{epsg} is left
#'   empty it defaults to the map unit of \code{aoi.}
#' @param dy A numeric specifying the resolution of the output raster in y dimension
#'   expressed in the map unit specified in \code{epsg}.
#'   If \code{epsg} is left empty it defaults to the map unit of \code{aoi}.
#' @param dt Resolution of the time dimension expressed as ISO8601 string
#'   (e.g. \code{"P16D"} for 16 days).
#' @param srs Target spatial reference system as a string; can be a proj4 definition,
#'   WKT, or in the form "EPSG:XXXX" (see \code{\link[gdalcubes]{cube_view}})
#' @param bbox A numeric vector of lenght four indicating the spatial bounding
#'   box of the query (xmin, ymin, xmax, ymax) in geographic coordinates. Defaults
#'   to `NULL` which will set the spatial extent to global, i.e. \code{c(-180, -90, 180, 90)}.
#' @param aggregation A character vector specifying the aggregation method for
#'   images of the same time stamp. Must be one of \code{"min"}, \code{"max"},
#'   \code{"mean"}, \code{"median"}, or \code{"first"}. Defaults to \code{"median"}.
#' @param resampling A character specifying the resampling method used to transform
#'   images from their native CRS to the output CRS. Can be any of the methods
#'   supported by gdalwarp (see \url{www.gdal.org/programs/gdalwarp.html}).
#'   Defaults to \code{"bilinear"}.
#' @param after A length one character vector in the form `%Y-%m-%d` specifying
#'   the earliest date (inclusive) included in the temporal extent.
#' @param before A length one character vector in the form `%Y-%m-%d` specifying
#'   the latest date (inclusive) included in the temporal extent.
#' @param tmpagg A logical indicating if temporal aggregation of all timestemps should
#'   be performed calling \code{\link[gdalcubes]{reduce_time}}. Defaults to \code{FALSE}.
#' @param index A character specifying the short name of the index to be calculated.
#'   Available indices can be checked with \code{check_indices()}. Defaults
#'   to NULL. When indices are calculated argument bands must be set to NULL.
#' @param bands A character vector specifying the bands to extract. Defaults to
#'   NULL. If specified argument index must be set to NULL.
#' @param label A character label which is appended to the output files in the
#'   form of \code{<label>_<index>_<timestep>}. Files with the same name in
#'   \code{outdir} will be overwritten without warning.
#' @param outdir A character specifying an existing directory where output files
#'   will be written to. Defaults to \code{"."}.
#' @param mask_layer Name of a layer in the data cube used for masking.
#' @param mask_values An integer vector with mask values. Masking behaviour
#'   is controlled with the \code{mask_invert} argument.
#' @param mask_invert If TRUE the values specified in \code{mask_values} are
#'   considered valid and pixels with these values are kept. If FALSE pixel
#'   with these values are masked out. Default is FALSE.
#' @param overwrite A logical indicating if existing files in outdir should be
#'   overwritten.
#' @param chunking Vector of length 3 defining the size of data cube chunks in the
#'   order time, y, x. (see \code{\link[gdalcubes]{raster_cube}})
#' @param threads Number of cores used for parallel processing
#' @param verbose  A logical indicating the verbosity.
#' @param ... additional arguments to  \code{\link[gdalcubes]{write_tif}}
#'
#'
#' @return Nothing. However a \code{GTiff} and a \code{GPKG} with the zonal statistics are written to
#'   \code{outdir} for every resulting timestep.
#' @note See \code{\link[gdalcubes]{cube_view}} for more information.
#' @export .calc_index
#' @keywords internal
#' @importFrom gdalcubes create_image_collection extent cube_view raster_cube select_bands apply_pixel reduce_time write_tif image_mask gdalcubes_options
#' @importFrom sf st_crs st_transform st_zm st_bbox
#' @importFrom stringr str_extract_all str_remove str_replace_all
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
#'
.calc_index <- function(
  files,
  format = "mapme.vegetation/inst/sentinel2_l2a_cog.json",
  DB_path = NULL,
  dx = NULL,
  dy = NULL,
  dt = NULL,
  srs = NULL,
  bbox = NULL,
  aggregation = "median",
  resampling = "bilinear",
  after = NULL,
  before = NULL,
  tmpagg=FALSE,
  index = NULL,
  bands = NULL,
  label = NULL,
  outdir = ".",
  mask_layer = "SCL",
  mask_values = c(3,8,9),
  mask_invert = FALSE,
  overwrite = F,
  chunking = c(1, 256, 256),
  threads = 1,
  verbose = TRUE,
  ...){

  . = NULL # for devtools::check() not throwing errors
  if (!aggregation %in% c("min", "max", "mean", "median", "first")){
    stop(paste0('Selected aggregation method for images of the same timestamp not supported\n',
                'Choose one of "min", "max", "mean", "median", or "first"'))
  }

  if(!is.null(index)){
    if(!is.null(bands)){
      stop("One of index or bands must be set to NULL.")
    }
  }

  gdalcubes_options(parallel = threads, progress = verbose)

  if(is.null(DB_path)){
    col = create_image_collection(files, format)
  } else if(file.exists(DB_path)){
    col = image_collection(DB_path)
  } else {
    col = create_image_collection(files, format, DB_path)
  }

  if(is.null(bbox)){
    message("No bbox specified. Setting spatial extent to the extent of the image collection.")
    bbox = as.data.frame(matrix(data = as.numeric(gdalcubes::extent(col)[1:4]),
                                nrow = 2))
    bbox2 = matrix(data =
                     c(bbox[1,1], bbox[2,2],
                       bbox[2,1], bbox[2,2],
                       bbox[2,1], bbox[1,2],
                       bbox[1,1], bbox[1,2],
                       bbox[1,1], bbox[2,2]),
                   nrow = 5, byrow=T)
    bbox2 = st_sfc(st_polygon(list(bbox2)))
    sf::st_crs(bbox2) = sf::st_crs(4326)
    bbox2 = st_transform(bbox2, st_crs(srs))
    bbox = st_bbox(bbox2)
  }

  ext = .update_ext(col, bbox = bbox, after = after, before = before)


  if(!is.null(index)){
    indices = read.csv(system.file("indices_mapme_vegetation.csv", package = "mapme.vegetation"))
    # alternative formula
    indices$alt_formula = indices$s2_formula
    for(i in 1:nrow(indices)) indices$alt_formula[i] = str_replace_all(indices$alt_formula[i], "band_", "B")

    if (!any(index %in% indices$name)){
      stop("Specified index cannot be calculated. Chose another one.")
    }

    indices$name = as.character(indices$name)
    i_target = c()
    i = sapply(index, function(name) which(indices$name == name))
    # i = which(indices$name %in% index)
    term = paste0("(", indices$alt_formula[i], ")")
    needed_bands = unique(str_extract_all(paste(term, collapse = " "), "B([0-9]{1,2})")[[1]])
    # bands_2 = unique(str_extract_all(paste(term, collapse = " "), "B(1[0-9])")[[1]])
    bands_3 = unique(str_extract_all(paste(term, collapse = " "), "B(8a)")[[1]])
    selected_bands = c(needed_bands, bands_3)

  }


  cubeview = cube_view(extent = ext,
                       srs = srs,
                       dx = dx,
                       dy = dy,
                       dt = dt,
                       aggregation = aggregation,
                       resampling = resampling)

  if(verbose){
    message("Current cube view is:")
    print(cubeview)
  }

  outnames = file.path(outdir, paste0(label, dimension_values(cubeview)$t, ".tif", sep = ""))

  if(any(file.exists(outnames)) & !overwrite){
    stop("Some output files exist in outdir and overwrite == FALSE")
  }

  mask = image_mask(mask_layer, values = mask_values, invert = mask_invert)

  if(!is.null(index)){
    # initiate cube and calculate index
    raster_cube(col, cubeview, mask = mask, chunking = chunking) %>%
      select_bands(selected_bands) %>%
      apply_pixel(term, names = sort(indices$name[i])) %>%
      {if(tmpagg) reduce_time(., paste0("mean(", sort(indices$name[i]), ")")) else . } -> cube

  } else { # extract bands

    raster_cube(col, cubeview, mask = mask, chunking = chunking) %>%
      select_bands(bands) %>%
      {if(tmpagg) reduce_time(., paste0("mean(", sort(indices$name[i]), ")")) else . } -> cube

  }

  band_order = bands(cube)$name
  time_vec = dimension_values(cube)$t
  if(nchar(time_vec[1]) == 7 )  time_vec = paste(time_vec, "-01", sep = "")

  cube %>%
    write_tif(dir = outdir, prefix = label, ...)
  # cube %>%
  #   write_ncdf(fname = file.path(outdir, paste0(label, ".nc")), , with_VRT = TRUE, ...)

  rm(col, cubeview); gc()
  list(files = outnames, band_order = band_order, time = time_vec)
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
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
check_indices <- function(){
  indices = read.csv(system.file("indices_mapme_vegetation.csv", package = "mapme.vegetation"))
  return(indices[ ,c(2:5)])
}
