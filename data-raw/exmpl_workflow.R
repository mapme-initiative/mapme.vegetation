library(sf)
library(raster)
library(stringr)
library(gdalUtils)
library(R.utils)
library(parallel)
library(sen2r)
library(sen2tool)
library(gdalcubes)
library(exactextractr)
link_sen2cor("~/sen2cor")

aoi = st_read(system.file("extdata", "testregion.gpkg", package = "sen2tool"))
aoi = st_transform(aoi, st_crs(32642)) # transform to UTM

test1 <- downloadS2(aoi,
                    time_window = c("2016-06-01", "2019-09-30"),
                    time_period = "seasonal",
                    extent_name = "testregion",
                    rootdir = ".",
                    max_cloud = 50,
                    use_db = TRUE,
                    force_update = FALSE)

test2 = transTifS2(safedirs = test1,
                   datadir = "data/testregion/L1C",
                   outdir = "data/testregion/tifs", # either relative to datadir or absolute path with parent directory existing
                   ncores = 1,
                   sen2cor = "~/sen2cor/")

#r = raster(test2[1])
#aoi = st_transform(aoi, st_crs(r))

test3 = cloudRemovalS2(datadir = "data/testregion/L1C",
                       outdir = "data/testregion/clouds",
                       tifdir = "data/testregion/tifs",
                       safedirs = test1,
                       tiffiles = test2,
                       aoi = aoi,
                       buffer = 100)

test4 = calcIndicesS2(indices = "NDVI",
                      outdir = "data/testregion/indices",
                      files = test3,
                      ncores = 4,
                      sen2cor = "~/sen2cor")

test5 = evalVI(vifiles = test4,
               pre_years = c("2016", "2017"),
               post_years = c("2018", "2019"),
               ncores = 6,
               name = "NDVI",
               aoi = aoi,
               returnRaster = TRUE,
               rasterdir = "inst/extdata",
               extent_name = "testregion")

st_write(test5, "inst/extdata/testregion_data.gpkg")

