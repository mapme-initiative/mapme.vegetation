#' Download S2 L2A COGs from AWS servers
#'
#' This function can be used for a spatiotemporal query of S2 L2A data on AWS
#' servers and downloads either all or specific assets found at the STAC API
#' endpoint. Internally the function depends on the `rstac` package which
#' makes queries to STAC endpoints very convinient. Note, currently only S2 L2A
#' COGs can be downloaded using this function.
#'
#' @param collection A name of a collection. Defaults to \code{"sentinel-s2-l2a-cogs"},
#'   currently the only supported collection.
#' @param assets A character vector indicating the assets to download, e.g. bands.
#'   Defaults to \code{NULL} which will download all available assets.
#' @param bbox A numeric vector of lenght four indicating the spatial bounding
#'   box of the query \code{c(xmin, ymin, xmax, ymax)} in geographic coordinates. Defaults
#'   to \code{NULL} which will set the spatial extent to global, i.e. \code{c(-180, -90, 180, 90)}.
#' @param after A length one character vector in the form `%Y-%m-%d` specifying
#'   the earliest date (inclusive) included in the temporal extent.
#' @param before A length one character vector in the form `%Y-%m-%d` specifying
#'   the latest date (inclusive) included in the temporal extent.
#' @param timeframe A length one character vector indicating whether the query
#'   should return the complete time series between \code{after} and \code{before} or
#'   if the query should be based on seasons. Possbile values are \code{"full"} and \code{"seasonal"}.
#'   Defaults to \code{"full"}.
#' @param max.cloud A length one integer indicating the maximum number of cloud
#'   cover in percent to qualify a scene for download. Defaults to \code{"100"} which will
#'   download all scenes matching the spatiotemporal extent.
#' @param max.items A length one integer indicating the maximum number of items
#'   to return from a single query. Defaults to 500. Can be used for cases where
#'   large queries will lead to long query times. But if that is the case it is
#'   advisable to break up into small sub-queries so that the complete data can
#'   be downloaded.
#' @param outdir A character vector of an existing directory. Defaults to the
#'   current working directory. Throws an error if the directory does not exist.
#' @param overwrite A logical indicator indicating if present data sets in \code{outdir}
#'   should be overwritten. Defaults to \code{FALSE}.
#' @param use_aria A logical if the aria2 downloader shall be used. Defaultes to
#'   FALSE which will download the items sequentially via GET requests. If TRUE
#'   it is expected that the argument \code{aria_bin} points towards a valid
#'   aria binary installation.
#' @param aria_bin A charachter vector pointing to a valid aria binary.
#' @param skip_existing A logical indicating if existing files should be removed from
#'   the download list prior to downloading with aria. Might speed up the process
#'   when a prior download was interrupted, however, when files were not downloaded
#'   completley their download is not ressumed (check for .aria files in outdir).
#'   Defaults to FALSE which means that existing files are not skipped.
#' @param max_concurrent Maximum number of connections to the server to load
#'   a single file. Defaults to \code{4}.
#' @param max_connections Maximum number of parallel connections to the server
#'   to load files simoltaniously. Defaults to 4.
#' @param retry Integer. Maximum integer of retries for erroneous downloads.
#' @param verbose A logical indicating the verbosity.
#' @param query_only Logical. Defaults to FALSE which means matching items are downloaded.
#'   Else matching items are returned in a list object.
#'
#' @return Nothing. This function downloads matching assets of the spatiotemporal
#'   query to the specified directory in \code{outdir}.
#' @export
#' @importFrom rstac stac stac_version
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
download_aws <- function(collection = "sentinel-s2-l2a-cogs",
                         assets = NULL,
                         bbox = NULL,
                         after = NULL,
                         before = NULL,
                         timeframe = "full",
                         max.cloud = 100,
                         max.items = 500,
                         outdir = ".",
                         overwrite = FALSE,
                         use_aria = F,
                         aria_bin = NULL,
                         skip_existing = FALSE,
                         max_concurrent = 4,
                         max_connections = 4,
                         retry = 2,
                         verbose = T,
                         query_only = F){


  # TODO: check inputs
  if(max.cloud < 0 | max.cloud > 100){
    stop("Argument max.cloud must be between 0 and 100.")
  }

  if(collection != "sentinel-s2-l2a-cogs"){
    message("Another collection than S2 L2A COGs was specified.")
    message("It is very likely that the queried data cannot be downloaded.")
    message("The function will continue anyway...")
  }

  # connect to aws stac catalog
  url = "https://earth-search.aws.element84.com/v0"
  s = stac(url)
  if(verbose) message(paste0("STAC version: ", stac_version(s))) # outputs current stac version

  # check if selected collection is available on endpoint
  .check_collection(s, collection)

  if(is.null(bbox)){
    if(verbose) message("No spatial extent specified. Setting to global")
    bbox = c(-180, -90, 180, 90)
  }
  if(class(bbox) == "bbox"){
    bbox = as.numeric(bbox)
  }

  # check if datetime can be coerced to date
  datetime = .check_dates(after, before, timeframe)

  # query spatiotemporal user domain by iterating through rows of datetime
  items = lapply(1:nrow(datetime), function(i) {
    temporal_window = paste0(datetime$after[i], "/", datetime$before[i])
    if(verbose) message(paste0("Querying data for temporal window ", temporal_window))
    query = .query_items(s, collection, bbox, temporal_window, max.items, verbose, max.cloud)

    if(!query_only){
      if(verbose) message("Starting download...")
      .download_assets(items = query,
                                     assets =  assets,
                                     outdir =  outdir,
                                     use_aria = use_aria,
                                     aria_bin = aria_bin,
                                     skip_existing = skip_existing,
                                     max_concurrent = max_concurrent,
                                     max_connections = max_connections,
                                     retry = retry,
                                     verbose = verbose)
    }
    query
  })
  items
}
