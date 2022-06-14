library(shiny)
library(tidyverse)
library(data.table)
library(sf)
library(DT)
library(emo)

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
    select(ID, link, state_name, city_name, name,
           difficulty_rating, num_reviews, num_photos,
           home_distance, length, elevation_gain, avg_grade,
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

# TODO: improve slider
ui = fluidPage(
    titlePanel("Hike Recommender"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("max_distance",
                        "Max distance:",
                        min = 1,
                        max = 500,
                        value = 150),
          sliderInput("max_length",
                      "Max length (mi):",
                      min = 1,
                      max = 50,
                      value = 7.5),
          sliderInput("max_elevation",
                      "Max elevation gain (ft):",
                      min = 1,
                      max = 10000,
                      value = 2000),
          sliderInput("max_duration",
                      "Max Hike Duration:",
                      min = 10,
                      max = 60*10,
                      value = 60*4)
        ),
        mainPanel(
           uiOutput('all_trail_hover_info'),
          
           
           # TODO: figure out way to not rerender map on zoom changes
           # TODO: center & align this better
           fluidRow(splitLayout(cellWidths = c('40%', '60%'),
                                plotOutput("all_trail_map",
                                           # width = 800, 
                                           # height = 500,
                                           hover = hoverOpts("all_trail_hover", delay = 50, delayType = "debounce")
                                           ),
                                plotOutput("city_map",
                                           width = 800, height = 600
                                           )
                             )
                   ),
           
           # TODO: see if its possible to highlight point on map from selection on data table
           dataTableOutput('cluster_table', width = '75%')
           )
        )
      )

# input = list(max_distance = 150,
#              max_length = 7.5,
#              max_elevation = 2000,
#              max_duration = 60*4)

server = function(input, output) {
    all_trails = fread('C:/Users/jeffb/Desktop/Life/personal-projects/hike-recommender/trails/all_trails_weighted.csv') |> 
      mutate(description = str_c(word(description, start = 1, end = 20, sep = fixed(" ")), '...')) |> 
      mutate(description = ifelse(is.na(description), '', description))
    
    states = map_data('state')
  
    # have this just show all trails with highlighted values + size for selections
    output$all_trail_map = renderPlot({
      
      all_trails_selection = Select_Clusters(all_trails, input$max_distance, input$max_length, input$max_elevation, input$max_duration) |> 
        left_join(all_trails |> select(ID, long, lat), by = 'ID') |> 
        st_as_sf(coords = c('long', 'lat'), crs = 4326)
      states_selection = states |> filter(region %in% str_to_lower(unique(all_trails_selection$state_name)))
      
      all_trails_plot = all_trails_selection |> 
        # st_as_sf(coords = c('long', 'lat'), crs = 4326) |>
        ggplot() + 
        geom_polygon(data = states_selection, aes(x=long, y=lat, group=group),
                    color="black", fill="gray99" ) +
        geom_sf(data = all_trails_selection |> filter(cluster_rank <= 10), 
                aes(color = cluster_rank), size = 5) +
        geom_sf(data = all_trails_selection |> filter(cluster_rank <= 10), 
                shape = 1, color = 'black', size = 5.5,
                inherit.aes = FALSE) +
        geom_sf(data = all_trails_selection |> filter(cluster_rank > 10),
                color = 'gray70', size = 2) +
        scale_color_continuous(low = 'dodgerblue',
                               high = 'gray95') +
        theme_void() +
        theme(legend.position = 'none')
      
        all_trails_plot
    })
    
    
    output$all_trail_hover_info = renderText({
      all_trails_selection = Select_Clusters(all_trails, input$max_distance, input$max_length, input$max_elevation, input$max_duration) |> 
        left_join(all_trails |> select(ID, long, lat), by = 'ID')
      
      hover = input$all_trail_hover
      point = nearPoints(all_trails_selection, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      # if (nrow(point) == 0) return(NULL)
     
      placeholder = HTML(paste0("<br/>",
                                "<br/>",
                                "<br/>",
                                "<br/>"))
      # refactor?
      if (nrow(point) == 0) {
        placeholder
      } else if (point$cluster_rank > 10) {
        placeholder
      } else if (point$cluster_rank <= 10) { 
        HTML(paste0("#", str_pad(point$cluster_rank, width = 2, pad = '0'), "<b> ", point$name, "</b>", "<br/>",
                    "<b>", emo::ji('right_arrow'), " </b>️", round(point$length, 2), " miles <br/>",
                    "<b>", emo::ji('up_arrow'), " </b>️", round(point$elevation_gain, 0), " feet <br/>",
                    "<b>", emo::ji('up_right_arrow'), " </b>️", round(point$avg_grade, 2), "% <br/>"))
      }
    })
    
    output$city_map = renderPlot({
      all_trails_selection = Select_Clusters(all_trails, input$max_distance, input$max_length, input$max_elevation, input$max_distance) |> 
        select(ID, cluster_rank)

      all_trails_combined = all_trails |> 
        left_join(all_trails_selection |> select(ID, cluster_rank), by = 'ID', keep = TRUE) |> 
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
    })
    
    output$trail_map = renderPlot({
      all_trails_selection = Select_Clusters(all_trails, input$max_distance, input$max_length, input$max_elevation, input$max_distance) |> 
        select(ID, cluster_rank)
      
      
      all_trails_combined = all_trails |> 
        left_join(all_trails_selection |> select(ID, cluster_rank), by = 'ID', keep = TRUE) |> 
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
    })
    
    
    # TODO: figure out way to apply selection once
    output$cluster_table = renderDataTable({
      tbl = Select_Clusters(all_trails, input$max_distance, input$max_length, input$max_elevation, input$max_distance) |> 
        select(-ID, -name, -cluster_rank) |>
        rename(state = state_name,
               city = city_name,
               difficulty = difficulty_rating,
               reviews = num_reviews,
               photos = num_photos) |> 
        mutate(across(c(home_distance:cluster_weight), ~round(., 3)))
      
      DT::datatable(tbl, escape = FALSE,
                    options = list(
                      autoWidth = TRUE,
                      columnDefs = list(list(width = '50px', targets = "_all")))
                   )
    })
}

shinyApp(ui = ui, server = server)
