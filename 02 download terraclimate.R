
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, elevatr, terra, climateR, openxlsx, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

library(AOI)
library(terra)
library(climateR)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1, encoding = 'latin1')

# Load data ---------------------------------------------------------------

# lbls <- tibble(season = c(rep('Otono', 4), rep('Primavera', 3), rep('Verano', 3)), months = c('Mar', 'Abr', 'May', 'Jun', 'Sep', 'Oct', 'Nov', 'Dic', 'Ene', 'Feb'))
shpf <- st_read('./proj_data/gpkg/base/prov_sampled.gpkg')
pnts <- read_excel('D:/01_maxent_data/proj_data/bd_bjm/bd_presencias.xlsx')
# pnts$basename

startDate <- as.Date('2016-01-01', format = '%Y-%m-%d')
endDate <- as.Date('2019-12-31', format = '%Y-%m-%d')

# To download -------------------------------------------------------------
prec <- climateR::getTerraClim(AOI = shpf, varname = 'ppt', startDate = startDate, endDate = endDate)
prec <- prec[[1]]

tmax <- climateR::getTerraClim(AOI = shpf, varname = 'tmax', startDate = startDate, endDate = endDate)
tmax <- tmax[[1]]

tmin <- getTerraClim(AOI = shpf, varname = 'tmin', startDate = startDate, endDate = endDate)
tmin <- tmin[[1]]

tavg <- (tmax + tmin) / 2

# To make the crop --------------------------------------------------------
prec <- terra::crop(prec, vect(shpf))
prec <- terra::mask(prec, vect(shpf))

tmax <- terra::crop(tmax, vect(shpf))
tmax <- terra::mask(tmax, vect(shpf))

tmin <- terra::crop(tmin, vect(shpf))
tmin <- terra::mask(tmin, vect(shpf))

tavg <- terra::crop(tavg, vect(shpf))
tavg <- terra::mask(tavg, vect(shpf))

# To write ----------------------------------------------------------------
dout <- './proj_data/tif/terraclimate/extent'
dir_create(dout)

terra::writeRaster(x = prec, filename = glue('{dout}/prec_allyears.tif'))
terra::writeRaster(x = tmax, filename = glue('{dout}/tmax_allyears.tif'))
terra::writeRaster(x = tmin, filename = glue('{dout}/tmin_allyears.tif'))
terra::writeRaster(x = tavg, filename = glue('{dout}/tavg_allyears.tif'))

# To check duplicated cells -----------------------------------------------
mask <- tavg[[1]] * 0 + 1
clls <- terra::extract(mask, pnts[,c('coor_x', 'coor_y')], cells = T)

table(clls[,1]) %>% length()
colnames(pnts)
pnts$idcodyceps %>% table()

# To download the elevation -----------------------------------------------
shpf <- sf::st_zm(shpf)
srtm <- get_elev_raster(shpf, z = 10)
srtm <- rast(srtm)
srtm <- terra::crop(srtm, shpf)
srtm <- terra::mask(srtm, shpf)
terra::writeRaster(x = srtm, filename = './proj_data/tif/srtm/srtm_150m_raw.tif')

clls <- terra::extract(srtm, pnts[,c('coor_x', 'coor_y')], cells = T)
table(clls[,1]) %>% length()
table(clls$cell) %>% as.data.frame() %>% arrange(desc(Freq))
