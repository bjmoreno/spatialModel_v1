
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, elevatr, terra, gtools, climateR, openxlsx, RSAGA, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1, encoding = 'latin1')

# Load data ---------------------------------------------------------------
path <- './proj_data/tif/terraclimate/extent/individual'
fles <- dir_ls(path)
fles <- grep('.tif$', fles, value = T)
fles <- as.character(fles)
fles <- grep('150', fles, value = TRUE)

# To check precipitation --------------------------------------------------
prec <- grep('prec', fles, value = T)

# Dtes <- c(paste0('2019-0', 4:9), paste0('2019-', 10:12))
# Dtes <- c(paste0('2018-0', 1:9), paste0('2019-', 10:12))
Dtes <- c(
  paste0('2016-0', 1:9), paste0('2016-', 10:12),
  paste0('2017-0', 1:9), paste0('2017-', 10:12),
  paste0('2018-0', 1:9), paste0('2018-', 10:12))
prec <- grep(paste0(Dtes, collapse = '|'), fles, value = TRUE)
prec <- grep('prec', prec, value = T)
prec <- map(.x = prec, .f = terra::rast)

prec <- map(.x = 1:length(prec), .f = function(i){
  cat(i, '\t')
  ppt <- prec[[i]]
  ppt[which.lyr(ppt < 0)] <- 0
  out <- glue('{path}/{names(ppt)}_v2.tif')
  terra::writeRaster(x = ppt, filename = out, overwrite = TRUE)
  cat('Done!\n')
})

# Check check - precipitation ----------------------------------------------

prec <- grep('prec.*v2\\.tif$', fles, value = T)
print(prec)


prec <- terra::rast(prec)

# To check tmax and tmin --------------------------------------------------
tmax <- grep('tmax', fles, value = TRUE)
tmin <- grep('tmin', fles, value = TRUE)

# tmax <- map(tmax, terra::rast)
# tmin <- map(tmin, terra::rast)

# Check tmax < tmin -------------------------------------------------------

purrr::map(.x = 1:length(tmax), .f = function(i){
  
  cat(i, '\t')
  tmx <- tmax[[i]]
  tmx <- terra::rast(tmx)
  tmn <- tmin[[i]]
  tmn <- terra::rast(tmn)
  stk <- c(tmn, tmx)
  tbl <- terra::as.data.frame(stk, xy = TRUE)
  rm(stk)
  nms <- colnames(tbl)[3:4] 
  colnames(tbl)[3:4] <- c('tmin', 'tmax')
  tbl <- as_tibble(tbl)
  tbl <- mutate(tbl, tmax = ifelse(tmin >= tmax, tmin, tmax))
  # tbl <- mutate(tbl, comparison = tmin <= tmax) # unique(tbl$comparison)
  rst <- terra::rast(tbl[,1:4], type = 'xyz')
  names(rst) <- nms
  rm(tbl)
  
  # To write
  dout <- unique(dirname(fles))
  terra::writeRaster(x = rst[[1]], filename = glue('{dout}/{names(rst[[1]])}_v2.tif'), overwrite = TRUE)
  terra::writeRaster(x = rst[[2]], filename = glue('{dout}/{names(rst[[2]])}_v2.tif'), overwrite = TRUE)
  rm(rst); gc()
  cat('Done!\n')
  
})

#optimized code

purrr::map(.x = 1:length(tmax), .f = function(i) {
  
  cat(i, '\t')
  
  tmx_file <- tmax[[i]]
  tmx_rast <- terra::rast(tmx_file)
  
  tmn_file <- tmin[[i]]
  tmn_rast <- terra::rast(tmn_file)
  
  stk <- c(tmn_rast, tmx_rast)
  tbl <- terra::as.data.frame(stk, xy = TRUE)
  rm(stk); gc()
  
  nms <- colnames(tbl)[3:4] 
  colnames(tbl)[3:4] <- c('tmin', 'tmax')
  
  tbl <- as_tibble(tbl)
  tbl <- mutate(tbl, tmax = ifelse(tmin >= tmax, tmin, tmax))
  
  rst <- terra::rast(tbl[,1:4], type = 'xyz')
  names(rst) <- nms
  rm(tbl); gc()
  
  dout <- unique(dirname(fles))
  
  tryCatch({
    terra::writeRaster(x = rst[[1]], filename = glue('{dout}/{names(rst[[1]])}_v2.tif'), overwrite = TRUE)
  }, error = function(e) {
    cat("Error al escribir el primer raster:", e$message, "\n")
  })
  
  tryCatch({
    terra::writeRaster(x = rst[[2]], filename = glue('{dout}/{names(rst[[2]])}_v2.tif'), overwrite = TRUE)
  }, error = function(e) {
    cat("Error al escribir el segundo raster:", e$message, "\n")
  })
  
  rm(rst, tmx_rast, tmn_rast); gc()
  cat('Done!\n')
  
})

