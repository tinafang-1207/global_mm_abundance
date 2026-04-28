


library(ncdf4)
library(terra)
library(dplyr)

url <- "https://psl.noaa.gov/thredds/dodsC/Datasets/COBE2/sst.mon.mean.nc"
nc <- nc_open(url)

# inspect actual dimension order
sapply(nc$var$sst$dim, function(x) x$name)
sapply(nc$var$sst$dim, function(x) x$len)

# read coordinates
lon  <- ncvar_get(nc, "lon")
lat  <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")

# convert time to dates
time_units <- nc$dim$time$units
origin_date <- sub("days since ", "", time_units)
dates <- as.Date(time, origin = origin_date)

# U.S. West Coast in 0-360 longitude
lon_min <- 230
lon_max <- 250
lat_min <- 20
lat_max <- 55

lon_idx <- which(lon >= lon_min & lon <= lon_max)
lat_idx <- which(lat >= lat_min & lat <= lat_max)

# read spatial subset but ALL time
# assumes dim order = lon, lat, time
sst_sub <- ncvar_get(
  nc,
  varid = "sst",
  start = c(min(lon_idx), min(lat_idx), 1),
  count = c(length(lon_idx), length(lat_idx), -1)
)

nc_close(nc)

# convert lon from 0-360 to -180 to 180
lon_sub <- lon[lon_idx]
lon_sub_clean <- ifelse(lon_sub > 180, lon_sub - 360, lon_sub)

lat_sub <- lat[lat_idx]

# sort lon increasing
lon_order <- order(lon_sub_clean)
lon_sub_clean <- lon_sub_clean[lon_order]
sst_sub <- sst_sub[lon_order, , ]

# sort lat increasing
lat_order <- order(lat_sub)
lat_sub_clean <- lat_sub[lat_order]
sst_sub <- sst_sub[, lat_order, ]

# reorder array to [time, lat, lon]
sst_sub <- aperm(sst_sub, c(3, 2, 1))

dim(sst_sub)
# should now be 2114 x nlat x nlon

##########################################################
# clean to tidyverse dataframe

nt   <- dim(sst_sub)[1]
nlat <- dim(sst_sub)[2]
nlon <- dim(sst_sub)[3]

idx <- expand.grid(
  time_id = seq_len(nt),
  lat_id  = seq_len(nlat),
  lon_id  = seq_len(nlon)
)

sst_df <- data.frame(
  lon  = lon_sub_clean[idx$lon_id],
  lat  = lat_sub_clean[idx$lat_id],
  date = dates[idx$time_id],
  sst  = as.vector(sst_sub)
) %>%
  arrange(date, lat, lon)

saveRDS(sst_df, "data/habitat_gis/sst_cobe2_us_west_coast.rds")












