
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, elevatr, terra, gtools, climateR, openxlsx, RSAGA, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1, encoding = 'latin1')

# Load data ---------------------------------------------------------------
path <- './proj_data/tif/terraclimate/extent'
fles <- dir_ls(path) %>% as.character()

# Read as a raster
prec <- grep('prec\\.tif$', fles, value = T) %>% terra::rast()
# tmax <- grep('tmax\\.tif$', fles, value = T) %>% terra::rast()
# tmin <- grep('tmin\\.tif$', fles, value = T) %>% terra::rast()
# 
# dir_ls(path)

# Read as a raster
# Read as a raster
prec <- fles %>% 
  .[grepl('prec_allyears\\.tif$', .) & !grepl('prec_allyears\\.tif.aux.json$', .)] %>% 
  terra::rast()

tmax <- fles %>% 
  .[grepl('tmax_allyears\\.tif$', .) & !grepl('tmax_allyears\\.tif.aux.json$', .)] %>% 
  terra::rast()

tmin <- fles %>% 
  .[grepl('tmin_allyears\\.tif$', .) & !grepl('tmin_allyears\\.tif.aux.json$', .)] %>% 
  terra::rast()

dir_ls(path)

# Dates -------------------------------------------------------------------
startDate <- as.Date('2016-01-01', format = '%Y-%m-%d')
endDate <- as.Date('2019-12-31', format = '%Y-%m-%d')
Dates <- seq(startDate, endDate, by = 'months')
Dates <- str_sub(Dates, 1, 7)

# Write each individual raster --------------------------------------------
dout <- glue('{path}/individual')
dir_create(dout)

for(i in 1:length(Dates)){
  cat(Dates[i], '\t')
  terra::writeRaster(x = prec[[i]], filename = glue('{dout}/prec_{Dates[i]}.tif'))
  terra::writeRaster(x = tmax[[i]], filename = glue('{dout}/tmax_{Dates[i]}.tif'))
  terra::writeRaster(x = tmin[[i]], filename = glue('{dout}/tmin_{Dates[i]}.tif'))
}

# List the files again ----------------------------------------------------
fles <- dir_ls('./proj_data/tif/terraclimate/extent/individual') %>% as.character() %>% mixedsort()
# fles <- fles[-grep('gwr_raw', fles, value = F)]
fles <- grep('.tif$', fles, value = T)
srtm <- './proj_data/tif/srtm/srtm_150m_raw_filled.tif'
envr <- rsaga.env(path = 'D:/01_maxent_data/saga/saga-9.1.1_x64')

#verificar datos de directorios
print(file.exists(fInp))
print(file.exists(srtm))


purrr::map(.x = 1:length(fles), .f = function(i){
  
  cat(basename(fles[i]), '\t')
  fInp <- fles[i]
  fOut <- glue('{dirname(fInp)}/{basename(fInp) %>% gsub(".tif", "_gwr_raw_150m.tif", .)}')
  rsaga.geoprocessor(lib = 'statistics_regression', module = 'GWR for Grid Downscaling', param = list(PREDICTORS = srtm, REGRESSION = fOut, DEPENDENT = fInp), env = envr)
  cat('Done\n')

})



