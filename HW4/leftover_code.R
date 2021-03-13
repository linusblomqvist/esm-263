# Leftover code

############ OLD CODE
# Create a 1000 ft (304.8 m) buffer around streams
stream_buffer <- st_buffer(streams, 304.8)

# Calculate NDVI
NDVI <- (landsat$Landsat71.4 - landsat$Landsat71.3) / (landsat$Landsat71.4 + landsat$Landsat71.3)

# Dummy for pixels with the requisite NDVI
suitable_NDVI <- NDVI >= 0.2 & NDVI <= 0.5

# Turn this into a single multipolygon
tic()
suitable_NDVI_polygon <- rasterToPolygons(suitable_NDVI, fun = function(x) {x == 1},
                                          dissolve = TRUE)
toc() # forever

suitable_NDVI_polygon <- st_as_sf(suitable_NDVI, as_points = FALSE, merge = TRUE)

# Create a new geometry, the intersection between watersheds and the stream buffer
watershed_stream_buffer_int <- st_intersection(watersheds, stream_buffer)

# Calculate area of each intersection
watershed_stream_buffer_int$shed_buf_area <- st_area(watershed_stream_buffer_int)

# Calculate riparian area for each watershed
riparian_area_by_watershed <- watershed_stream_buffer_int %>%
  st_drop_geometry() %>% 
  select(shed_buf_area, objectid) %>%
  group_by(objectid) %>%
  summarize(tot_int_area = sum(shed_buf_area))

# Join the riparian buffer area onto the watershed dataframe
watersheds <- left_join(watersheds, riparian_area_by_watershed, by = "objectid")

# Calculate percentage riparian
watersheds$pct_riparian <- watersheds$tot_int_area / st_area(watersheds)

# Calculate z score to normalize
watersheds <- mutate(watersheds, z_score = (pct_riparian - mean(pct_riparian)) / sd(pct_riparian))

###### NDVI
