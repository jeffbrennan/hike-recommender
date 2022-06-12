# data cleaning
library(tidyverse)
library(data.table)
library(jsonlite)
library(glue)

# clustering
library(sp)
library(rgdal)
library(geosphere)

# mapping / viz
library(sf)
library(ggpubr)
library(ggrepel)
library(ggmap)
library(osmdata)

# map colors
rec_color = '#05bbaa'     #teal
nonrec_color = '#FB836F'  #salmon

# extract relevant data from nested list cols and flatten
Get_Data = function(fname) { 
  result = fromJSON(fname)[['hits']] |>
    group_by(row_number()) |> 
    mutate(backpacking = 'backpacking' %in% unlist(activities)) |> 
    mutate(camping = 'camping' %in% unlist(activities)) |> 
    mutate(water_stuff = str_detect(paste0(activities), 'sea-kayaking|kayaking|paddle-sports|canoeing')) |>
    mutate(lat = `_geoloc`[[1]]) |> 
    mutate(long = `_geoloc`[[2]]) |> 
    ungroup() |> 
    select(-activities, -`_geoloc`, -units, -objectID,
           -duration_minutes_hiking,
           -duration_minutes_mountain_biking,
           -duration_minutes_cycling)
  return(result)
}


all_trails_combined |> filter(duration_minutes != duration_minutes_hiking) |> View()

# https://stackoverflow.com/questions/32100133/print-the-time-a-script-has-been-running-in-r
hms_span = function(start, end) {
  dsec = as.numeric(difftime(end, start, unit = "secs"))
  hours = floor(dsec / 3600)
  minutes = floor((dsec - 3600 * hours) / 60)
  seconds = dsec - 3600*hours - 60*minutes
  paste0(
    sapply(c(hours, minutes, seconds), function(x) {
      formatC(x, width = 2, format = "d", flag = "0")
    }), collapse = ":")
}

# get distance matrix between symmetrical lat/lon vectors
Calc_Distance = function(lon, lat, id=NA) {
  if (length(id) == 1) {
    id = rep('1', times = length(lon))
  } else { 
    message('Generating ', length(lon), 'x', length(lon), ' distance matrix...\n')
    }

  result = tryCatch(
    {
      xy = SpatialPointsDataFrame(
        matrix(c(lon, lat), ncol=2),
        data.frame(ID = id),
        proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
      
      distance_matrix = distm(xy) |> 
        as.data.frame() |> 
        mutate(across(everything(), ~ ./ 1609)) |> 
        as.matrix()
      
      if (nrow(distance_matrix) == 2) {
        as.vector(distance_matrix[2,1])
      } else {
        list('xy' = xy,
             'distance_matrix' = distance_matrix)  
      }
    },
    error=function(cond) {
      return(NA)
    }
  )  
  return(result)
}


# TODO: consider multicore processing with one matrix per contiguous region if decide to add more states
# 7836 trails -> 61 mil dist values 
# Cluster trails by creating nxn matrix of distances in miles
Minimize_Clusters = function(initial_cluster, cluster_locations_nearest, min_trail = 3) { 

    minimized_clusters_raw = initial_cluster |> 
    left_join(cluster_locations_nearest |> select(clust, nearest_distance, nearest_cluster), by = 'clust') |> 
    group_by(clust) |> 
    mutate(below_threshold = n() < 5) |> 
    ungroup()
  
  minimized_clusters_below = minimized_clusters_raw |> 
    filter(below_threshold) |> 
    arrange(clust) |> 
    mutate(clust_appearance = map(nearest_cluster, ~which(clust == .)[1])) |> 
    mutate(clust = ifelse(row_number() > clust_appearance | is.na(clust_appearance), nearest_cluster, clust))
  
  minimized_clusters = minimized_clusters_raw |> 
    filter(!below_threshold) |> 
    plyr::rbind.fill(minimized_clusters_below) |>
    arrange(clust) |> 
    group_by(clust) |> 
    mutate(clust = cur_group_id()) |> 
    ungroup() |> 
    select(-nearest_distance, -nearest_cluster, -clust_appearance, -below_threshold)
  
  stray_trails = minimized_clusters |>
    group_by(clust) |>
    filter(n() < min_trail) |> 
    ungroup() 
  
  return(list('clust_df' = minimized_clusters,
              'stray_trails' = stray_trails))
}


Calc_Distance_Cluster = function(geo_df, cluster_results) { 
  cluster_locations = geo_df |>
    left_join(cluster_results, by = c('id' ='ID')) |> 
    group_by(clust) |> 
    summarize(across(c(lon, lat), median)) |> 
    ungroup()
  
  cluster_distm = Calc_Distance(cluster_locations$lon,
                                cluster_locations$lat,
                                id=cluster_locations$clust)[['distance_matrix']]
  
  cluster_locations_nearest = cluster_locations |> 
    group_by(clust) |>
    mutate(nearest_distance = sort(cluster_distm[, clust])[2]) |> 
    mutate(nearest_cluster = which(cluster_distm[, clust] == nearest_distance))
  
  return(cluster_locations_nearest)
}

# cluster groups of geocoded trails, reducing number of clusters if not enough trails
Get_Clusters = function(lon, lat, id, cluster_distance, min_trail=3) {
# initial run ---------------------------------------------------------------------------------
  
  message('Calculating all trail distances...')
  results = Calc_Distance(lon, lat, id)
  
  message('Performing initial cluster...')
  clusters = hclust(as.dist(results[['distance_matrix']]), method="complete")
  
  results[['xy']][['clust']] = cutree(clusters, h = cluster_distance)
  cluster_results = results[['xy']]@'data'
  
 
# minimize clusters ---------------------------------------------------------------------------
  geo_df = cbind(id, lon, lat) |>as.data.frame()
  cluster_locations_nearest = Calc_Distance_Cluster(geo_df, cluster_results)
  
  stray_trails = cluster_results |>
    group_by(clust) |> 
    filter(n() < min_trail)
  
  attempts = 1
  while(nrow(stray_trails) > 0 & attempts < 100) {
    message('[Attempt #', str_pad(attempts, width = 2, pad = '0'), '] ',
            str_pad(nrow(stray_trails), width = 4, pad = ' '),
            ' stray trails identified. Reducing number of clusters...')
    loop_results = Minimize_Clusters(cluster_results, cluster_locations_nearest, min_trail)
    cluster_results = loop_results[['clust_df']]
    stray_trails = loop_results[['stray_trails']]
    cluster_locations_nearest = Calc_Distance_Cluster(geo_df, cluster_results)
    attempts = attempts + 1
  }

  avg_trail_cluster = cluster_results |>
    group_by(clust) |>
    summarize(n = n()) |>
    pull(n) |>
    mean()
  
  message('Average trails per cluster: ', round(avg_trail_cluster, 2))
  return(cluster_results)
}

# cleaning ------------------------------------------------------------------------------------
## pull data ----------------------------------------------------------------------------------------

all_states = list.files('trails/', full.names = TRUE) %>% 
  .[str_detect(., '\\.json')]

all_trails_raw = map(all_states, Get_Data) |> rbindlist(fill = TRUE)

## clusters ------------------------------------------------------------------------------------
# obtain clusters of hikes in 25 mile radius
message('Obtaining hike clusters...')
start = Sys.time()
hike_clusters = Get_Clusters(all_trails_raw$long,
                             all_trails_raw$lat,
                             all_trails_raw$ID,
                             cluster_distance = 10,
                             min_trail = 3)
message('Time elapsed: ', hms_span(start, Sys.time()))

## geometry ------------------------------------------------------------------------------------
all_trails_sf =  st_as_sf(all_trails_raw, coords = c('long', 'lat'), crs = 4326) |> 
  select(ID, geometry)


## distance from apt ---------------------------------------------------------------------------
home_address = fread('backend/home.csv')

# dplyr 26 seconds
start_time = Sys.time()
# TODO: add capability to compute distance from any point
home_distance = all_trails_raw |> 
  select(ID, lat, long) |> 
  group_by(row_number()) |> 
  mutate(home_distance = Calc_Distance(c(lat, home_address[['lat']]),
                                       c(long, home_address[['lon']]))
  ) |> 
  ungroup() |> 
  select(-`row_number()`)
message('Time elapsed: ', hms_span(start_time, Sys.time()))







## combine -------------------------------------------------------------------------------------
all_trails_combined = all_trails_sf |> 
  left_join(all_trails_raw, by = 'ID') |> 
  left_join(hike_clusters |> rename(distance_cluster = clust),
            by = 'ID') |> 
  left_join(home_distance |> select(ID, home_distance), by = 'ID') |>
  mutate(avg_grade = (elevation_gain / length) * 100) |>
  mutate(length = length / 1609) |> 
  mutate(elevation_gain = elevation_gain * 3.281) |> 
  mutate(difficulty_rating = factor(difficulty_rating, labels = c('easy', 'med', 'hard', '💀'))) |> 
  mutate(name = str_squish(name)) |> 
  mutate(ID = as.character(ID)) |> 
  mutate(created_at_ymd = as.Date(created_at / 60 / 60 / 24, origin = '1970-01-01'))


## weighting -----------------------------------------------------------------------------------
range01 = function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

# TODO: make dynamic based on whether easy or hard hike is desired
weighting_vars = list('pos' = c('popularity', 'num_reviews', 'num_photos', 'avg_rating'),
                      'neg' = c('duration_minutes', 'avg_grade', 'created_at'))



all_trails_weight = all_trails_combined |>
  st_drop_geometry() |> 
  select(all_of(c('ID', 'route_type', weighting_vars[['pos']], weighting_vars[['neg']]))) |>
  mutate(route_type_pref = case_when(route_type == 'O' ~ 0.3,
                                     route_type == 'L' ~ 0.7,
                                     TRUE ~ 0)) |> 
  mutate(across(where(is.numeric), range01, na.rm = TRUE)) |>
  mutate(pos_weight = rowSums(across(weighting_vars[['pos']]))) |> 
  mutate(neg_weight = rowSums(across(weighting_vars[['neg']]))) |> 

  # TODO: add actual weights instead of just summing values
  mutate(weight = pos_weight - neg_weight)

  
all_trails = all_trails_combined |> 
  left_join(all_trails_weight |> select(ID, pos_weight, neg_weight, weight), by = 'ID')
  
# maps -----------------------------------------------------------------------------------------
states = map_data('state')



## all trails ----------------------------------------------------------------------------------
# TODO: add florida, CT, RI, oregon
all_trails |> 
ggplot() + 
  geom_polygon( data=states, aes(x=long, y=lat, group=group),
                color="black", fill="gray95" ) + 
geom_sf(color = 'dodgerblue') + 
theme_void()


## cluster testing --------------------------------------------------------------------------------
all_trails |> 
  filter(state_name == 'New York') |>
  filter(home_distance < 25) |> 
  group_by(distance_cluster) |>
  filter(n() > 5) |> 
  mutate(distance_cluster = cur_group_id()) |> 
  ungroup() |> 
  mutate(distance_cluster = as.character(distance_cluster)) |>
  mutate(distance_cluster = str_pad(distance_cluster, width = 3, pad = '0')) |> 
  select(geometry, distance_cluster) |> 
  ggplot() + 
  geom_polygon(data = states |> filter(region == 'new york'), aes(x=long, y=lat, group=group),
                color = "black", fill="gray95") + 
  geom_sf(aes(color = distance_cluster)) + 
  theme_void()

# exploration ---------------------------------------------------------------------------------

# elevation & length
all_trails |>
  filter(duration_minutes_hiking < 600) |>
  ggplot(aes(x = length, y = elevation_gain)) + 
  geom_point(aes(color = difficulty_rating)) + 
  labs(y = 'elevation gain (feet)',
       x = 'length (mi)') +
  facet_wrap(~state_name) +
  theme_pubr()

ggsave(filename = 'viz/scatter_elevation_length.png', width = 9, height = 9, dpi = 600)

# compare difficulty by state
all_trails |>
  filter(duration_minutes_hiking < 600) |>
  mutate(difficulty_rating = factor(difficulty_rating, labels = c('easy', 'med', 'hard', '💀'))) |> 
  ggplot(aes(x = avg_grade, y = state_name)) + 
  geom_boxplot(aes(fill = difficulty_rating)) + 
  facet_wrap(~difficulty_rating, ncol = 1) +
  labs(y = '') +
  theme_pubr() + 
  theme(legend.position = 'none')

ggsave(filename = 'viz/box_difficulty_grade.png', width = 6, height = 12, dpi = 600)

all_trails |> 
  select(avg_grade, length, elevation_gain, duration_minutes_hiking, state_name) |>
  filter(length < 20) |> 
  st_drop_geometry() |> 
  pivot_longer(!state_name) |> 
  ggplot(aes(x = value, y = state_name)) + 
  geom_boxplot(aes(fill = name)) + 
  facet_wrap(~name, ncol = 1, scales = 'free') +
  labs(x = '', y = '') +
  theme_pubr() + 
  theme(legend.position = 'none')

ggsave(filename = 'viz/box_trail_features.png', width = 6, height = 12, dpi = 600)

# recommendation weighting -----------------------------------------------------------------------
# weighted component based on 
# 1 - popularity or num reviews (test this)
# 2 - time to complete
# 3 - avg grade


# in shiny app, given number of days & max hiking time, filter and recommend top suggestions
# visualize selections against other hikes in the same area with scatter plot of elevation and length

# need to obtain combinations of hikes within a cluster, prioritizing most desireable
max_distance = 50
max_length = 10 
max_elevation = 5000
max_duration = 60 * 4 
Select_Clusters = function(df, max_distance, max_length, max_elevation, max_duration, state = NA) { 
  # TODO: refactor with mutate across
  if(!is.na(state)){df = df |> filter(state_name %in% state)}
  
  ranks = df |>
    st_drop_geometry() |> 
    select(ID, distance_cluster, weight, home_distance, length, elevation_gain, duration_minutes) |> 
    group_by(distance_cluster) |> 
    filter(min(home_distance) <= max_distance) |>
    ungroup() |> 
    filter(length <= max_length) |>
    filter(elevation_gain <= max_elevation) |>
    filter(duration_minutes <= max_duration) |>
    group_by(distance_cluster) |> 
    arrange(distance_cluster, desc(weight)) |> 
    mutate(length_cumsum = cumsum(length)) |>
    mutate(elevation_cumsum = cumsum(elevation_gain)) |>
    mutate(duration_cumsum = cumsum(duration_minutes)) |>
    mutate(length_flag = length_cumsum <= max_length * 1.1) |> 
    mutate(elevation_flag = elevation_cumsum <= max_elevation * 1.1) |> 
    mutate(duration_flag = duration_cumsum <= max_duration * 1.1) |> 
    filter(length_flag & elevation_flag & duration_flag) |> 
    mutate(cluster_weight = mean(weight)) |>
    ungroup() |> 
    mutate(cluster_rank = dense_rank(desc(cluster_weight)))
  
  result_df = df |>
    inner_join(ranks |> select(ID, cluster_weight, cluster_rank), by = 'ID') |> 
    select(ID, state_name, city_name, name,
           difficulty_rating, num_reviews, num_photos,
           home_distance, length, elevation_gain,
           cluster_weight, cluster_rank,
           description)
}


recs = Select_Clusters(all_trails,
                       max_distance = 150,
                       max_length = 10,
                       max_elevation = 7500,
                       max_duration = 60 * 4)



# recommendation viz testing ------------------------------------------------------------------
## maps ----------------------------------------------------------------------------------------
Get_Bounds = function(df, zoom_level = 8) {
  
  zoom_to = c(mean(df$long), mean(df$lat))  # Berlin
  zoom_level = zoom_level
  lon_span = 360 / 2^zoom_level
  lat_span = 180 / 2^zoom_level
  
  lon_bounds = c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
  lat_bounds = c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)
  
  return(list('lon' = lon_bounds,
              'lat' = lat_bounds))
  
}

Get_Basemap = function(df, box_zoom, map_zoom) { 
  bbox = make_bbox(df$long, df$lat,
                   f = box_zoom)
  
  basemap = get_map(location = bbox,
                    zoom = map_zoom,
                    source = 'osm') |> 
    suppressMessages()
  
  return(basemap)
}

Plot_Map = function(df, basemap, fname){ 
  map = ggmap(basemap) +
    geom_sf(data = df |> filter(!rec),
            color = nonrec_color, size = 3,
            inherit.aes = FALSE) + 
    geom_sf(data = df |> filter(rec),
            color = rec_color, size = 5,
            inherit.aes = FALSE) + 
    geom_sf(data = df |> filter(rec),
            shape = 1, color = 'black', size = 5.5,
            inherit.aes = FALSE) + 
    geom_label_repel(data = df |> filter(rec),
                     aes(label = name, x = long, y = lat),
                     color = 'white',  fill = rec_color, 
                     size = 4, fontface = "bold",
                     inherit.aes = FALSE) +
    theme_void()
  
  return(map)
}

CLUSTER = 1
rec_test = all_trails |> 
  filter(distance_cluster == CLUSTER) |> 
  arrange(desc(popularity)) |> 
  mutate(rec = row_number() <= 5)


trail_basemap = Get_Basemap(rec_test |> filter(rec),
                            box_zoom = 0.5, 
                            map_zoom = 14)


city_basemap = Get_Basemap(rec_test |> filter(rec),
                           box_zoom = 5, 
                           map_zoom = 10)

trail_detail = Plot_Map(rec_test, trail_basemap, 'trail')
ggsave(glue('viz/cluster_{CLUSTER}_trail_detail.png'), plot = trail_detail, dpi = 600, width = 8, height = 8)

city_detail = Plot_Map(rec_test, city_basemap, 'trail')
ggsave(glue('viz/cluster_{CLUSTER}_city_detail.png'), plot = city_detail, dpi = 600, width = 8, height = 8)


# comparison vs cluster
rec_test |>
  ggplot(aes(x = length, y = elevation_gain)) + 
  geom_point(data = rec_test |> filter(rec), size = 5, alpha = 1,
             aes(color = difficulty_rating)) +
  geom_point(data = rec_test |> filter(!rec), color = 'gray80', size = 2, alpha = 0.5)  +
  geom_text(data = rec_test |> filter(rec), aes(label = name)) +
  labs(y = 'elevation gain (feet)',
       x = 'length (mi)') +
  facet_wrap(~state_name) +
  theme_pubr()

# comparison vs state
rec_test_state = all_trails |>
  filter(state_name == rec_test$state_name[1]) |> 
  mutate(rec = ID %in% (rec_test |> filter(rec) |> pull(ID)))

rec_test_state |> 
  ggplot(aes(x = length, y = elevation_gain)) + 
  geom_point(data = rec_test_state |> filter(rec), size = 5, alpha = 1,
             aes(color = difficulty_rating)) +
  geom_point(data = rec_test_state |> filter(!rec), color = 'gray80', size = 2, alpha = 0.5)  +
  geom_text(data = rec_test_state |> filter(rec), aes(label = name)) +
  labs(y = 'elevation gain (feet)',
       x = 'length (mi)') +
  facet_wrap(~state_name) +
  theme_pubr()
