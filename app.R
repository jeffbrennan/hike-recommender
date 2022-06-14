library(shiny)
library(data.table)
library(glue)

# geocoding
library(sp) # home distance
library(geosphere)
library(tmaptools)

# viz
# plots
library(sf)
library(tidyverse)
library(ggrepel)
library(ggmap)

# table
library(DT)

# map colors
rec_color = '#05bbaa'     #teal
nonrec_color = '#FB836F'  #salmon

Calc_Distance = function(lon, lat) {
  return(distm(matrix(c(lat, lon), ncol = 2))[2,1])
}

# geocode test
Select_Clusters = function(df, max_distance, max_length, max_elevation, max_duration, state = NA) { 
  if(!is.na(state)){df = df |> filter(state_name %in% state)}
  
  # TODO: refactor with mutate across
  ranks = df |>
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
    select(ID, lat, long, link, state_name, city_name, name,
           difficulty_rating, num_reviews, num_photos,
           duration_minutes, home_distance, length, elevation_gain, avg_grade,
           cluster_weight, cluster_rank,
           description) |> 
    filter(cluster_rank <= 100) |>
    arrange(cluster_rank) |> 
    ungroup()
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

ui = fluidPage(
  # tags$head(tags$style(HTML("div#inline label { width: 32%; }
  #                              div#inline input { display: inline-block; width: 68%;}"))),
  # tags$head(
  #   tags$style(type="text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle; }
  #                                  #inline .form-group { display: table-row;}")),
    fluidRow(
      column(6, offset = 3,
             align = 'center',
             style = glue("background-color:{rec_color}"),
             h1("Hike Recommender")
            )
    ),
    fluidRow(
      column(6, offset = 3,
             fluidRow(
               column(6,
                      sliderInput("difficulty_choices",
                                  "Hike Difficulty",
                                  min = 1,
                                  max = 7,
                                  step = 2, 
                                  value = 5,
                                  ticks = FALSE,
                                  width = '100%')
                      ),
               column(2, ''),
               column(4, align = 'right',
                      textInput("starting_location",
                                "Enter a starting location",
                                value = '',
                                width = '100%')
                      )
             )
      )),
    fluidRow(
      column(6, offset = 3,
             fluidRow(
               column(6,
                      sliderInput("max_distance",
                                  "Driving Distance (mi): ",
                                  min = 1,
                                  max = 500,
                                  value = 150,
                                  width = '100%',
                                  ticks = FALSE)
               ),
               column(6, align = 'right',
                      sliderInput("max_duration",
                                  "Hike Duration (min): ",
                                  min = 10,
                                  max = 60*10,
                                  value = 60*4,
                                  width = '100%',
                                  ticks = FALSE)
               )
             )
      )
    ),
    fluidRow(
      column(6, offset = 3,
             fluidRow(
               column(6,
                      sliderInput("max_length",
                                  "Hike Length (mi): ",
                                  min = 1,
                                  max = 50,
                                  value = 7.5,
                                  width = '100%',
                                  ticks = FALSE)
               ),
               column(6, align = 'right',
                      sliderInput("max_elevation",
                                  "Elevation gain (ft): ",
                                  min = 1,
                                  max = 10000,
                                  value = 2000,
                                  width = '100%', 
                                  ticks = FALSE)
                      )
               )
             )
  ),
  # main panel
  fluidRow(
    column(10, offset = 1,
           fluidRow(
             column(5, align = 'center',
                    style = glue("background-color:{nonrec_color}"),
                    uiOutput('all_trail_hover_info')
             )),
           fluidRow(
            column(5, align = 'left',
                   style = glue("background-color:{rec_color}"),
                   plotOutput("all_trail_map",
                              hover = hoverOpts("all_trail_hover", delay = 50, delayType = "debounce")
                              ),
                   ),
            column(2, ''),
            column(5,
                   style = glue("background-color:{nonrec_color}"),
                   plotOutput("city_map")
                   )
           )
  )),
    fluidRow(
      column(10, offset = 1,
             style = glue("background-color:{nonrec_color}"),
             dataTableOutput('cluster_table')
           )
      )
)

server = function(input, output) {
    all_trails_raw = fread('C:/Users/jeffb/Desktop/Life/personal-projects/hike-recommender/trails/all_trails_weighted.csv') |> 
      mutate(description = str_c(word(description, start = 1, end = 60, sep = fixed(" ")), '...')) |> 
      mutate(description = ifelse(is.na(description), '', description))
    
    all_trails = reactive({
      if(input$starting_location != '') { 
        home_address = tmaptools::geocode_OSM(input$starting_location)[['coords']]
        all_trails_raw |>
          rowwise() |> 
          mutate(home_distance =  Calc_Distance(c(lat, home_address[['y']]),
                                                c(long, home_address[['x']]))) |> 
          ungroup() |> 
          mutate(home_distance = home_distance / 1609)
      } else {
        all_trails_raw
        }
    })
    
    all_trails_selection = reactive({
      Select_Clusters(all_trails(), input$max_distance, input$max_length, input$max_elevation, input$max_distance)
    })
    
    # have this just show all trails with highlighted values + size for selections
    output$all_trail_map = renderPlot({
      if (nrow(all_trails_selection()) > 0) { 
    
        states = map_data('state')
        
        states_selection = states |>
          filter(region %in% (all_trails_selection() |>
                                pull(state_name) |>
                                unique() |>
                                str_to_lower())
                 )
        
        all_trails_selection_sf = all_trails_selection() |> 
          st_as_sf(coords = c('long', 'lat'), crs = 4326) |> 
          select(cluster_rank, geometry)
        
        all_trails_plot =  all_trails_selection_sf |> 
          ggplot() + 
          geom_polygon(data = states_selection, aes(x=long, y=lat, group=group),
                      color="black", fill="gray99" ) +
          geom_sf(data = all_trails_selection_sf |> filter(cluster_rank <= 10), 
                  aes(color = cluster_rank), size = 5) +
          geom_sf(data = all_trails_selection_sf |> filter(cluster_rank <= 10), 
                  shape = 1, color = 'black', size = 5.5,
                  inherit.aes = FALSE) +
          geom_sf(data = all_trails_selection_sf |> filter(cluster_rank > 10),
                  color = 'gray70', size = 2) +
          scale_color_continuous(low = 'dodgerblue',
                                 high = 'gray95') +
          theme_void() + 
          theme(plot.background = element_rect(fill = 'gray90')) +
          theme(legend.position = 'none')
        
          all_trails_plot
        
      }
    })
    
    output$all_trail_hover_info = renderText({
      if (nrow(all_trails_selection()) > 0) { 
          
        hover = input$all_trail_hover
        point = nearPoints(all_trails_selection(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        placeholder = HTML(paste0(rep('<br/>', times = 2), sep = ''))
                           
        if (nrow(point) == 0) {
          placeholder
        } else if (point$cluster_rank > 10) {
          placeholder
        } else if (point$cluster_rank <= 10) { 
          HTML(paste0("#", str_pad(point$cluster_rank, width = 2, pad = '0'), "<b> ", point$name, "</b>", "<br/>",
                      "<b>",  round(point$length, 2), " mi </b>️" ,emo::ji('right_arrow'),
                      " <b>", round(point$elevation_gain, 0), " ft </b>️", emo::ji('up_arrow'),
                      " <b>", round(point$avg_grade, 2), "% </b>", emo::ji('up_right_arrow')))
        }
      }
    })
    
    output$city_map = renderPlot({
      if (nrow(all_trails_selection()) > 0) {
      
        all_trails_combined = all_trails() |> 
          left_join(all_trails_selection() |> select(ID, cluster_rank), by = 'ID', keep = TRUE) |> 
          mutate(rec = !is.na(ID.y)) |> 
          rename(ID = ID.x) |> 
          select(-ID.y)
        
        best_cluster = all_trails_combined |>
          filter(cluster_rank == min(cluster_rank, na.rm = TRUE)) |> 
          pull(distance_cluster)
        
        # TODO: pls refactor
        all_trails_out = all_trails_combined |> 
          filter(distance_cluster == best_cluster) |> 
          st_as_sf(coords = c('long', 'lat'), crs = 4326, remove = FALSE)
          
        city_basemap = Get_Basemap(all_trails_out,
                                   box_zoom = 3, 
                                   map_zoom = 10)
        
        city_detail = Plot_Map(all_trails_out, city_basemap, 'trail')
        city_detail
        
      }
    })
    
    output$trail_map = renderPlot({
      if (nrow(all_trails_selection()) > 0) { 
        
      all_trails_combined = all_trails() |> 
        left_join(all_trails_selection() |> select(ID, cluster_rank), by = 'ID', keep = TRUE) |> 
        mutate(rec = !is.na(ID.y))
      
      best_cluster = all_trails_combined |> filter(cluster_rank == min(cluster_rank, na.rm = TRUE)) |> pull(distance_cluster)
      
      # TODO: pls refactor
      all_trails_out = all_trails_combined |> 
        filter(distance_cluster == best_cluster) |> 
        st_as_sf(coords = c('long', 'lat'), crs = 4326, remove = FALSE)
      
      trail_basemap = Get_Basemap(all_trails_out,
                                  box_zoom = 0.5, 
                                  map_zoom = 14)
      
      trail_detail = Plot_Map(all_trails_out, trail_basemap, 'trail')
      trail_detail
      }
    })
    
    output$cluster_table = renderDataTable({
      if (nrow(all_trails_selection()) > 0) { 
        
        tbl = all_trails_selection() |> 
          select(link, state_name, city_name, difficulty_rating, duration_minutes, description) |>
          rowwise() |> 
          mutate(location = HTML(str_c(city_name, ',<br/>', state_name))) |> 
          ungroup() |> 
          select(-city_name, -state_name) |> 
          # select(-ID, -lat, -long, -name, -cluster_rank) |>
          rename(difficulty = difficulty_rating,
                 `hike time` = duration_minutes) |> 
          relocate(location, .after = 'link')
        # mutate(across(c(home_distance:cluster_weight), ~round(., 2)))
        
        DT::datatable(tbl, escape = FALSE,
                      options = list(
                        autoWidth = TRUE
                        # scrollX = TRUE,
                        # columnDefs = list(
                        #   list(width = '50px', targets = c(2)),
                        #   list(width = '100px', targets = c(6)),
                        #   list(width = '50px', targets = c(3:5))
                        #   )
                        )
                      )
        }
    })
}

# input = list(max_distance = 150,
#              max_length = 7.5,
#              max_elevation = 2000,
#              max_duration = 60*4)


shinyApp(ui = ui, server = server)
