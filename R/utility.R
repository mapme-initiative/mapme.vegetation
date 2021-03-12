#' Helper function to update tile index
#'
#' This function is a helper functions which downloads and updates the
#' tile index file from the Google Cloud. Users can choose if they want
#' to use a SQLite database which is going to be created in the local
#' installation path of the package or if they want to use the raw
#' csv file. The latter consumes more disk space (about 5 GB) but
#' speeds up future queries significantly, while the former
#' consumes less disk space (about 2 GB) but increases each query time.
#' The function additionally checks if the file is older than a month
#' and asks the user if they wish to update. Additionally, the
#' user can force an update by setting the corresponding option.
#'
#'
#' @param use_db A logical parameter indicating if the tile index
#'   should be saved locally as an SQLite database. This significantly
#'   speeds up future queries but also increases the local disk
#'   space used (from about 2 GB to 5 GB). Default is set to True.
#' @param force_update A logical parameter indicating that the
#'   latest version of the tile index should be downloaded from
#'   the Google Cloud.
#'
#' @return Nothing.
#' @export update_indexDB
#' @importFrom sqldf sqldf
#' @importFrom utils askYesNo
#' @importFrom R.utils gunzip
#' @importFrom DBI dbConnect dbWriteTable
#' @importFrom RSQLite SQLite
#' @keywords internal
#' @author Darius Görgen, Dr. Fabian Löw (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
update_indexDB <- function(use_db = TRUE, force_update = F){

  # get package path
  path_to_package = find.package("sen2tool")
  if (!force_update){
    download_csv = F

    if (use_db){
      file = file.path(path_to_package, "tilesDB")
      if(!file.exists(file)){download_csv = TRUE}
      if(!download_csv & difftime(Sys.time(), file.info(file)$ctime, units = "weeks") > 4){
        download_csv = askYesNo("The local copy of the Sentinel-2 tile indices is older than a month. Do you wish do update? (This can take a while...)", default = FALSE)
      }
    }

    if(!use_db){
      file = file.path(path_to_package, "tiles.csv")
      if(!file.exists(file)){download_csv = TRUE}
      if(!download_csv & difftime(Sys.time(), file.info(file)$ctime, units = "weeks") > 4){
        download_csv = askYesNo("The local copy of the Sentinel-2 tile indices is older than a month. Do you wish do update? (This can take a while...)", default = FALSE)
      }
    }

  } else {

    download_csv = T
  }

  if(download_csv){ # download and process if update is selected
    message(paste0("Downloading tile index file from Google bucket to the package directory. (This can take a while...)"))
    download.file("https://storage.googleapis.com/gcp-public-data-sentinel-2/index.csv.gz", file.path(path_to_package, "/tiles.csv.gz"))
    gunzip(file.path(path_to_package, "tiles.csv.gz"))

    if(use_db){
      message("Creating SQLite data base. (This can take a while...)")
      # save current wd in variable
      org_wd = getwd()
      setwd(path_to_package)
      conn = dbConnect(SQLite(), dbname = "tilesDB")
      dbWriteTable(conn, "tiles", "tiles.csv", overwrite = T)
      file.remove("tiles.csv")
      setwd(org_wd)
    }
  }
}

