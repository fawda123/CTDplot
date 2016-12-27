sample(123)
PB_dep_pts <- readShapePoints('M:/GIS/PB_dep_pts.shp') %>%
  as.data.frame %>%
  filter(1:nrow(.) %in% sample(1:nrow(.), 2500, replace = F)) %>%
  arrange(POINTID) %>%
  rename(
    Depth = GRID_CODE,
    Long = coords.x1,
    Lat = coords.x2
  ) %>%
  select(-POINTID) %>%
  mutate(Depth = -1 * Depth)

save(PB_dep_pts, file = 'data/PB_dep_pts.RData', compress = 'xz')
