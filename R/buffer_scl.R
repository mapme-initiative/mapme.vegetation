#' Apply a buffer to SCL cloud mask for specific values
#'
#' This function allows to apply spatial buffers for SCL tiles in order to enrich
#' the cloud cover classification and exclude potentially degradated pixels.
#' It uses GRASS GIS for fast processing. User's will need to provide a working
#' GRASS installation in order to use this function. It can be applied in parallel
#' to further increase computation speed.
#'
#' @param scl_files A chrachter vector to the input SCL files.
#' @param mask_values An integer vector indiacating the values of the raster
#'   which are considered as values to mask. Note that the resulting output
#'   raster will get the first value for all pixels within the spatial buffer.
#' @param mask_buffer A numeric indicating the distance of the spatial buffer
#'   in map units (usuall meters).
#' @param grass_bin A charachter vector to the directory of the GRASS binaries.
#' @param outdir A charachter vector pointin towards a directory where the output
#'   files should be written to. Note that this should be a different directory
#'   from where the original files are stored because the output files will have
#'   identical names.
#' @param threads The number of threads for parallel computation.
#'
#' @return Nothing. Writes buffered SCL mask to outdir. All cells within the
#'   buffer will obtain the value of the first element in mask_values.
#' @export scl_buffer
#' @importFrom doSNOW registerDoSNOW
#' @importFrom snow makeCluster stopCluster
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom foreach foreach %dopar%
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3

scl_buffer <- function(
  scl_files,
  mask_values,
  mask_buffer,
  grass_bin,
  outdir,
  threads){

  if(threads>1){
    cl <- makeCluster(threads)
    registerDoSNOW(cl)

    pb <- txtProgressBar(min = 0, max = length(scl_files), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress=progress)

    foreach(i=1:length(scl_files), .combine=c, .options.snow=opts) %dopar% {
      .scl(scl_file = scl_files[i],
           outdir = outdir,
           mask_values = mask_values,
           mask_buffer = mask_buffer,
           grass_bin = grass_bin)
    }
    close(pb)
    stopCluster(cl)
  }
  else {
    pb = txtProgressBar(min = 0, max = length(scl_files), style = 3)
    for(i in 1:length(scl_files)){
      .scl(scl_file = scl_files[i],
           outdir = outdir,
           mask_values = mask_values,
           mask_buffer = mask_buffer,
           grass_bin = grass_bin)
      setTxtProgressBar(pb, i)
    }
  }


}


#' Helper function to apply SCL cloud mask buffer
#'
#' This function is used internally to buffer a given SCL cloud mask. All buffered
#' cells are set to the value of the first object in mask_values
#'
#' @param scl_file A chrachter vector indicating the path to the input SCL file
#' @param outdir A path to a directory where the output file will be written to.
#' @param mask_values A numeric vector indicating which values should be masked
#' @param mask_buffer The distance for the buffer in map units (typically in meters).
#' @param grass_bin A path to the GRASS GIS binary directory.
#'
#' @return Nothing. Writes buffered SCL mask to outdir. All cells within the
#'   buffer will obtain the value of the first element in mask_values.
#'
#' @export .scl
#' @keywords internal
#' @importFrom rgrass7 execGRASS
#' @importFrom link2GI findGRASS linkGRASS7
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
.scl <- function(
  scl_file,
  outdir,
  mask_values,
  mask_buffer,
  grass_bin
){

  outname = file.path(outdir, basename(scl_file))
  if(!file.exists(outname)){
    rgrass7::set.useInternOption(TRUE)
    tmpname = tempfile(fileext = ".tif")
    inname = scl_file
    term = paste(paste("1*(A==", mask_values, ")", sep = ""), collapse = "+")
    rcl_command = sprintf('gdal_calc.py -A %s --outfile=%s --calc="%s"',
                          inname, tmpname, term)
    system(rcl_command, intern = T)

    gisdb = tempfile()
    location = file.path(gisdb, "temploc")
    grass = findGRASS(grass_bin)
    grass = linkGRASS7(tmpname,
                       gisdbase = gisdb,
                       location = location,
                       default_GRASS7 = grass)
    execGRASS("r.import", input = tmpname, output = "tmpraster")
    execGRASS("r.buffer", input = "tmpraster", distances = mask_buffer, output = "buffered", flags = "z")
    writeLines(" 0 = NULL \n 1 = 1 \n 2 = 1", con = file.path(gisdb, "rules.txt"))
    execGRASS("r.reclass", input = "buffered", output = "rcl", rules = file.path(gisdb, "rules.txt"))
    execGRASS("r.out.gdal", input = "rcl", output = outname,
              type = "Byte", createopt = "COMPRESS=DEFLATE")
    execGRASS("g.remove", type = "all", flags = c("f", "quiet"), name = "rcl")
    execGRASS("g.remove", type = "all", pattern = "*", flags = c("f", "quiet"))
    unlink(gisdb)
    file.remove(tmpname)
  }
}
