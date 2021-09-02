#' Downloads S2 and Landsat data via Google BigQuery
#' 
#' This function sends a SQL query to the Google Servers requesting data sets
#' matching the spatiotemporal extent specified by the user.
#' Additionally, the search can be restricted to a specific spacecraft and the 
#' maximum cloud cover within a scene. The data is then downloaded to a specified
#' output directory. For S2 data, a simple cloud mask can be calculated on the
#' fly.
#'
#' @param bbox A numeric vector of lenght four indicating the spatial bounding
#'   box of the query (xmin, ymin, xmax, ymax) in geographic coordinates. Defaults
#'   to `NULL` which will set the spatial extent to global, i.e. `c(-180, -90, 180, 90)`.
#' @param after A length one character vector in the form `%Y-%m-%d` specifying
#'   the earliest date (inclusive) included in the temporal extent.
#' @param before A length one character vector in the form `%Y-%m-%d` specifying
#'   the latest date (inclusive) included in the temporal extent.
#' @param overwrite A logical indicator indicating if present data sets in `outdir`
#'   should be overwritten. Defaults to `FALSE`.
#' @param outdir A character vector of an existing directory. Defaults to the 
#'   current working directory. Throws an error if the directory does not exist.
#' @param billing A Google Cloud Plattform project to which the queried data amount 
#'   is billed to. Create one at Google's website or run `gcloud init` in your 
#'   command line.
#' @param bucket The bucket to query data from. Must be one of `sentinel_2_index` 
#'   or `landsat_index`.
#' @param spacecraft A character vector that can be used to filter matching data
#'   to a specific spacecraft. Defaults to '"all"' which will download data from
#'   all spacecrafts matching the spatiotemporal query. 
#' @param period A length one character vector indicating whether the query
#'   should return the complete time series between `after` and `before` or
#'   if the query should be based on seasons. Possbile values are `full` and `seasonal`.
#'   Defaults to `full`.
#' @param max.cloud A length one integer indicating the maximum number of cloud
#'   cover in percent to qualify a scene for download. Defaults to `100` which will
#'   download all scenes matching the spatiotemporal extent. 
#' @param create_cloudmask A logical indicating if a simple cloud mask should be
#'   calculated for S2 scenes. Defaults to `FALSE`. Argument has no effect when
#'   Landsat data is downloaded.
#' @param verbose A  logical indicating the verbosity.
#' @return Nothing. Downloads all data sets matching the spatiotemporal query to
#'   a output directory.
#' @export
#' @importFrom DBI dbConnect dbGetQuery
#' @importFrom bigrquery bigquery
#' @importFrom magrittr %<>%
#' @importFrom dplyr filter
download_bigquery <- function(bbox = NULL, 
                              after = NULL, 
                              before = NULL, 
                              overwrite = FALSE, 
                              outdir = ".", 
                              billing = NULL, 
                              bucket = NULL, 
                              spacecraft = "all",
                              period = "full",
                              max.cloud = 100,
                              create_cloudmask = F,
                              verbose = T){
  spacecraft_id = NULL
  if(is.null(bbox)){
    if(verbose) message("No spatial extent specified. Setting to global")
    bbox = c(-180, -90, 180, 90)
  }
  
  if(!bucket %in% c("landsat_index", "sentinel_2_index")) stop("Bucket not available.")
  
  if(is.null(billing)) stop("No project name for billing data traffic specified.")
  
  # establish connection to bigquery
  con <- dbConnect(
    bigquery(),
    project = "bigquery-public-data",
    dataset = "cloud_storage_geo_index",
    billing = billing
  )
  
  
  # check if datetime can be coerced to date
  datetime = .check_dates(after, before, period)
  
  for (i in 1:nrow(datetime)){
    if(verbose) message(paste0("Querying data for temporal window ", datetime$after[i], "/", datetime$before[i]))
    sql = paste0("SELECT * FROM `bigquery-public-data.cloud_storage_geo_index.",bucket,"`",
                 " WHERE north_lat >= ", bbox[1],
                 " AND south_lat <= ", bbox[3],
                 " AND west_lon <= ", bbox[2],
                 " AND east_lon >= ", bbox[4],
                 " AND cloud_cover <= ", max.cloud,
                 " AND DATE(sensing_time) >= '", datetime$after[i],"'",
                 " AND DATE(sensing_time) <= '", datetime$before[i], "'")
    
    items = dbGetQuery(con, sql)
    if(verbose) print(items)
    
    if(spacecraft == "all"){ # filter for spacecraft
      .download_bigquery(items, bucket, outdir)
    } else {
      items %<>%
        filter(spacecraft_id == spacecraft)
      .download_bigquery(items, bucket, outdir)
    }
  }
}


#' Internal downloader for bigquery
#' 
#' This fucntion is used internally to download S2 or Landsat data from a 
#' Google Bucket. When S2 data is downloaded missing directories on the Google
#' Bucket are added so that external software e.g. sen2cor is able to run on the
#' data. Additionally a simple cloud mask can be applied for S2 data sets.
#'
#' @param items An items tibble returned by BigQuery matching the spatiotemporal
#'   query. 
#' @param bucket A length one charachter vector indicating the data source. Must
#'  be either `sentinel_2_index` ord `landsat_index`.
#' @param outdir A character vector of an existing directory. Defaults to the 
#'   current working directory. Throws an error if the directory does not exist.
#' @param overwrite A logical indicator indicating if present data sets in `outdir`
#'   should be overwritten. Defaults to `FALSE`.
#' @param create_cloudmask A logical indicating if a simple cloud mask should be
#'   calculated for S2 scenes. Defaults to `FALSE`. Argument has no effect when
#'   Landsat data is downloaded.
#'
#' @return Nothing. Downloads matching data sets to `outdir`.
#' @export
#' @keywords internal
.download_bigquery = function(items, bucket, outdir, overwrite, create_cloudmask){
  
  if(!dir.exists(outdir)) stop("outdir does not exist.")
  
  file_names = basename(items$base_url)
  
  if (bucket == "sentinel_2_index"){
    
    for(i in 1:nrow(items)){
      if(file.exists(file.path(outdir, file_names[i])) & overwrite == FALSE){
        message(paste0("\n File ", file_names[i], " already exists in datadir. Skipping download."))
        next
      } else {
        
        # prepare command with gsutil
        command = paste0("gsutil -m cp -r ", items$base_url[i], " ", outdir)
        dir.create(outdir, showWarnings = F, recursive = T)
        # try to download and save output in s
        s = try(suppressWarnings(system(command, intern = T, ignore.stderr = T)))
        
        # some files are not present in GoogleCloud Bucket, thus the download fails
        # this is only the case for very few files
        if(!is.null(attributes(s)$status)){
          message(paste0("\n File ", file_names[i], " could not be downloaded."))
          
        } else {
          # creating some empty folders which are needed by sen2r/sen2cor
          # functionality to allow further processing
          dir.create(file.path(outdir, file_names[i], "AUX_DATA"), showWarnings = F)
          dir.create(file.path(outdir, file_names[i], "HTML"), showWarnings = F)
          message(paste0("\n File ", file_names[i], " succesfully downloaded."))
          if(create_cloudmask) createCloudMask(file.path(outdir, file_names[i]), overwrite = F)
        }
      }
    }
    
  } else { # landsat bucket
    
    for(i in 1:nrow(items)){
      if(file.exists(file.path(outdir, file_names[i])) & overwrite == FALSE){
        message(paste0("\n File ", file_names[i], " already exists in datadir. Skipping download."))
        next
      } else {
        # prepare command with gsutil
        command = paste0("gsutil -m cp -r ", items$base_url[i], " ", outdir)
        # try to download and save output in s
        s = try(suppressWarnings(system(command, intern = T, ignore.stderr = T)))
        # some files are not present in GoogleCloud Bucket, thus the download fails
        # this is only the case for very few files
        if(!is.null(attributes(s)$status)){
          message(paste0("\n File ", file_names[i], " could not be downloaded."))
          
        } else {
          message(paste0("\n File ", file_names[i], " succesfully downloaded."))
        }
        
      }
    }
  }
}
