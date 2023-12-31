
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, terra, openxlsx, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1, encoding = 'latin1')

# Load data ---------------------------------------------------------------
#path <- 'D:/01_maxent_data/proj_data/'
#argt <- glue('{path}/area_pais_arg/') %>% dir_ls(., regexp = '.shp$') %>% st_read()

#dir_ls(path) %>% as.character()

#  To make study area -----------------------------------------------------
dirs <- dir_ls('D:/01_maxent_data/proj_data/sh_tutoria_R_oct_2022/prov_sampled') %>% 
  grep('prov', ., value = TRUE) %>% 
  as.character() 
shpf <- map(dirs, dir_ls) %>% unlist() %>% grep('.shp', ., value = T) %>% as.character() %>% grep('polign', ., value = T)
shpf <- map(shpf, st_read)

for(i in 1:length(shpf)){
  plot(st_geometry(shpf[[i]]))
}

# Join all the areas into only one shapefile 
shpf <- bind_rows(shpf)
dout <- './proj_data/gpkg/base'
st_write(shpf, './proj_data/gpkg/base/prov_sampled.gpkg')

shpf <- st_read('./proj_data/gpkg/base/prov_sampled.gpkg')

# To join all the tables into only one ------------------------------------
#fles <- dir_ls(path) %>% grep('tabs', ., value = T) %>% as.character() %>% dir_ls() %>% grep('.xlsx$', ., value = T) %>% as.character()
#lbls <- tibble(season = c(rep('Otono', 4), rep('Primavera', 3), rep('Verano', 3)), months = c('Mar', 'Abr', 'May', 'Jun', 'Sep', 'Oct', 'Nov', 'Dic', 'Ene', 'Feb'))

#tbls <- purrr::map_dfr(.x = 1:length(fles), .f = function(i){
#   cat(basename(fles[i]), '\n')
#   tbl <- fles[i]
#   tbl <- read_excel(tbl)
#   nme <- basename(fles[i])
#   nme <- gsub('.xlsx', '.csv', nme)
#   tbl <- mutate(tbl, basename = nme)
#   tbl <- dplyr::select(tbl, basename, everything())
#   return(tbl)
# })

# dout <- './tble/points'
# write.xlsx(x = tbls, file = glue('{dout}/all_points.xlsx'))

#tbls

bd.bjm <- read_excel('D:/01_maxent_data/proj_data/bd_bjm/bd_presencias.xlsx')

# Table to points
pnts <- st_as_sf(bd.bjm, coords = c('coor_x', 'coor_y'), crs = st_crs(4326))

# To write this shapefile
st_write(pnts, './proj_data/gpkg/points.gpkg')
# 
# # To check the points 
# plot(st_geometry(argt))
# points(tbls$coor_x, tbls$coor_y, pch = 16, col = 'red')
# 
# plot(st_geometry(shpf))
# points(tbls$coor_x, tbls$coor_y, pch = 16, col = 'red')

