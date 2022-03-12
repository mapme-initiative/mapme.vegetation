#' Internal function to update a spatio temporal extent
#'
#' @param col A image collection
#' @param bbox A spatial bounding box
#' @param after A date character
#' @param before A date character
#'
#' @export .update_ext
#' @keywords internal
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
.update_ext <- function(col, bbox = NULL, after = NULL, before = NULL){

  ext = extent(col)

  if(!is.null(bbox)){
    ext$left = bbox[1]
    ext$right = bbox[3]
    ext$top = bbox[4]
    ext$bottom = bbox[2]
  }

  if(!is.null(after)){
    ext$t0 = after
  }

  if(!is.null(before)){
    ext$t1 = before
  }

  ext
}

#' Check specified dates and arrange for download
#'
#' This function is used to check if the user specified dates can be coerced
#' to dates. Additionally, it arranges the dates based on the user wants a full
#' time series or seasonal periods between different years.
#'
#' @param after A length one character vector in the form `%Y-%m-%d` specifying
#'   the earliest date (inclusive) included in the temporal extent.
#' @param before A length one character vector in the form `%Y-%m-%d` specifying
#'   the latest date (inclusive) included in the temporal extent.
#' @param timeframe A length one character vector indicating whether the query
#'   should return the complete time series between `after` and `before` or
#'   if the query should be based on seasons. Possbile values are `full` and `seasonal`.
#'   Defaults to `full`.
#'
#' @return A data-frame with one row for `timeframe = "full"` and multiple rows
#'   depending on the number of years between `after` and `before` if `timeframe = "seasonal"`.
#' @export
#' @keywords internal
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
.check_dates = function(after, before, timeframe){

  if(is.null(after) | is.null(before)){
    stop("Either `after` or `before` is NULL. Please specify a temporal extent.")
  }

  tryCatch(
    {
      after =  as.Date(after, "%Y-%m-%d")
      before = as.Date(before, "%Y-%m-%d")

    },
    error = function(cond){
      message("Object datetime cannot be coerced to date.")
      message("Please check the format.")
      message(cond)
      stop()
    }
  )

  if(!before > after){
    stop("Date in argument after is not before the date in argument before. Check dates.")
  }


  if(!timeframe %in% c("full", "seasonal")){
    stop("Object period must be one of 'full' or 'seasonal'")
  }

  if(timeframe == "full"){
    datetime = data.frame(after = after,
                          before = before)
  } else {

    after_vec = strftime(seq(after, before, by = "year"), "%Y-%m-%d")
    before_vec = strftime(rev(seq(before, after, by = "-1 year")), "%Y-%m-%d")

    datetime = data.frame(after = after_vec,
                          before = before_vec)

  }
  return(datetime)
}

#' Check availability of a collection
#'
#' This function checks if a user specified collection is available at the STAC
#' endpoint and stops with an error message if that is not the case
#'
#' @param s A STAC endpoint created with `rstac::stac()`.
#' @param collection A length one charachter vector indicating a collection name
#'
#' @return Nothing. The function is used to check the availablility of a collection.
#' @export
#' @keywords internal
#'
#' @importFrom rstac collections get_request
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
.check_collection = function(s, collection){

  collections = s %>% collections() %>% get_request()
  ids = unlist(lapply(collections$collections, function(x) x$id))

  if(!collection %in% ids){
    message(paste0("Collection ", collection, " not available at endpoint."))
    message(paste0("Available collections are:\n", paste(ids, collapse = "\n")))
    stop()
  }
}


#' Queries items from a STAC endpoint
#'
#' This function queries all items of a given collection of a STAC endpoint
#' matching a spatiotemporal extent.
#'
#' @param s A STACconnection object from the \code{rstac} package.
#' @param collection A length one character vector indicating the name of the
#'   collection to query for items.
#' @param bbox A numeric vector of length four indicating the spatial bounding
#'   box of the query \code{(xmin, ymin, xmax, ymax)} in geographic coordinates. Defaults
#'   to `NULL` which will set the spatial extent to global, i.e. \code{c(-180, -90, 180, 90)}.
#' @param datetime A length one character vector indicating the start and endpoint
#'   of the temporal query.
#' @param max.items A length one integer indicating the maximum number of items
#'   to return from a single query. Defaults to 500. Can be used for cases where
#'   large queries will lead to long query times. But if that is the case it is
#'   advisable to break up into small sub-queries so that the complete data can
#'   be downloaded.
#' @param verbose A  logical indicating the verbosity.
#' @param max.cloud A length one integer indicating the maximum number of cloud
#'   cover in percent to qualify a scene for download. Defaults to \code{100} which will
#'   download all scenes matching the spatiotemporal extent.
#'
#' @return A items list object of \code{rstac}.
#' @export .query_items
#' @keywords internal
#' @importFrom rstac stac_search %>% post_request
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
.query_items = function(s,
                        collection,
                        bbox,
                        datetime,
                        max.items,
                        verbose,
                        max.cloud) {
  q = s %>%
    stac_search(collections = collection,
                bbox = bbox,
                datetime = datetime,
                limit = max.items)
  q$params$query = paste0("{\"eo:cloud_cover\": {\"lt\": ", max.cloud, "}}")
  items = q %>% post_request()

  if(verbose){
    message("The following items matched the specified spatiotemporal extent.")
    print(items)
  }
  return(items)
}


#' Download items from a STAC catalog
#'
#' This function is used internally to download a given list of items matching
#' a spatiotemporal query.
#'
#' @param items A list of items returned by `rstac::stac_search()`
#' @param assets Default is NULL. A possible charachter vector to specify which
#'   assets to download. If assets do not match the function returns an error
#'   and lists the available assets.
#' @param outdir A character vector of an existing directory. Defaults to the
#'   current working directory.
#' @param use_aria A logical if the aria2 downloader shall be used. Defaultes to
#'   FALSE which will download the items sequentially via GET requests. If TRUE
#'   it is expected that the argument `aria_bin` points towards a valid
#'   aria binary installation.
#' @param aria_bin A charachter vector pointing to a valid aria binary.
#' @param skip_existing A logical indicating if existing files should be removed from
#'   the download list prior to downloading with aria. Might speed up the process
#'   when a prior download was interrupted, however, when files were not downloaded
#'   completley their download is not ressumed (check for .aria files in outdir).
#'   Defaults to FALSE which means that existing files are not skipped.
#' @param max_concurrent Maximum number of connections to the server to load
#'   a single file. Defaults to 4.
#' @param max_connections Maximum number of parallel connections to the server
#'   to load files simoltaniously. Defaults to 4.
#' @param retry Integer. Maximum integer of retries for erroneous downloads.
#' @param verbose A logical indicating the verbosity.
#'
#' @return Nothing. Downloads specified assets into `outdir`.
#' @export
#' @keywords internal
#'
#' @importFrom rstac assets_download items_bands
#' @importFrom stringr str_sub str_detect
#' @importFrom tools file_ext
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
.download_assets = function(
  items = NULL,
  assets = NULL,
  outdir = ".",
  use_aria = FALSE,
  aria_bin = NULL,
  skip_existing = FALSE,
  max_concurrent = 4,
  max_connections = 4,
  retry = 2,
  verbose = TRUE){

  if(!dir.exists(outdir)) stop("outdir does not exist.")

  # check availability for all requested assets
  if(!is.null(assets)){
    available_assets = items %>%
      items_bands()

    if(!all(assets %in% available_assets[[1]])){
      missing = assets[which(!assets %in% available_assets[[1]])]
      message(paste0("Asset(s) ", paste(missing, sep = ", "), " not available."))
      message(paste0("Available assets are:\n", paste(available_assets[[1]], collapse = " \n")))
      stop()
    }
  }

  if(use_aria){

    tryCatch(
      {
        aria_version = system(paste0(aria_bin, " --version"), intern = T)
      }, error =  function(cond){
        stop(paste0(aria_bin, " not a valid aria2 binary."))
      }
    )

    if(verbose) message(aria_version[1])

    urls = lapply(1:length(items$features), function(i){
      id = items$features[[i]]$id
      if(is.null(assets)){
        item_assets = items$features[[i]]$assets
      } else {
        item_assets = items$features[[i]]$assets[assets]
      }
      paths = lapply(1:length(item_assets), function(j){
        asset_name = names(item_assets)[j]
        asset_href = item_assets[[j]]$href
        fileext   = paste0(file_ext(asset_href))
        outfile = sprintf("%s/%s_%s.%s", outdir, id, asset_name, fileext)
        c(asset_href, paste0("  out=", outfile))
      })
      unlist(paths)
    })

    urls = unlist(urls)

    if(skip_existing){
      out_files = str_sub(urls[c(FALSE, TRUE)], 7, -1)
      existing_files = out_files[which(file.exists(out_files))]
      if(length(existing_files>0)){
        if(verbose) message(sprintf("Found %s existing files. Removing from download list.", length(existing_files)))

        index_out = as.numeric(sapply(existing_files, FUN = function(x) grep(x,urls)))
        index_href = index_out - 1
        index = c(index_href, index_out)
        urls = urls[-index]

        if(length(urls) == 0){
          stop("All files seem to exist in outdir. Setting skip_existing = FALSE might help.")
        }
      }
    }

    url_file = file.path(tempdir(), paste0(Sys.Date(), "-aria2-S2-urls.txt"))
    writeLines(urls, url_file)
    args = sprintf("--show-console-readout=false --console-log-level=warn --summary-interval=30 -c -j %s -x %s -s %s -i %s",
                      max_concurrent, max_connections, max_connections, url_file)
    rep = TRUE
    counter = 0
    while(rep){

      if(verbose) stdout = "" else stdout = TRUE
      options(warn=-1)
      aria_out = system2(aria_bin, args = args, stdout = stdout)
      options(warn=0)

      filenames = str_sub(urls[seq(2, length(urls), by = 2)], 7,-1)
      filenames = filenames[which(!file.exists(filenames))]

      if(length(filenames != 0)){
        if(verbose) message(sprintf("Could not download %s assets. Retrying %s times.",
                                    length(filenames), retry - counter))

        index = unlist(lapply(seq_along(filenames), function(i){
          index_out = which(str_detect(paste0("  out=", filenames[i]), urls))
          index_href = index_out -1
          c(index_href, index_out)
        }))

        urls = urls[index]
        writeLines(urls, url_file)
        if(sum(str_detect(aria_out, "ERR"))  == 1) rep = FALSE
        if(counter == retry) rep = FALSE
      } else {
        rep = FALSE
      }
      counter = counter + 1
    }

    if(verbose){
      if(length(filenames) == 0){
        if(verbose) message("Sucessfully downloaded all assets.")
      } else {
        file.copy(url_file, ".")
        message(sprintf("Could not download %s assets. Missing urls are found in working directory.",
                        length(filenames)))
      }
    }

  } else { # no aria

    if(is.null(assets)){
      if(verbose) message("No assets specified. Downloading all assets.")
      assets_download(items, output_dir = outdir, progress = verbose)

    } else {
      assets_download(items, assets_name = assets, output_dir = outdir, progress = verbose)
    }
  }
}

#' Internal function to check date vectors
#'
#' Function used across the package to check the input of date vectors.
#' Throws errors when times is not a character vector and if not all
#' elements can be coerced to dates.
#'
#' @param times A charachter vector representing dates
#'
#' @return Nothing
#' @export .check_times
#' @keywords internal
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
.check_times <- function(times){

  if(!is.character(times))(
    stop("Argument times must be charachter vector of form %Y-%m-%d")
  )

  tryCatch(
    lapply(times, as.Date),
    error = function(cond){
    stop("Could not coerce times to date object. Check input of argument times.")
    }
  )
}

