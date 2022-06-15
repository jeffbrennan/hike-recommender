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
# library(grid)
library(shinybrowser)
library(shinyjs)

# table
library(DT)

# map colors
rec_color = '#05bbaa'     #teal
nonrec_color = '#FB836F'  #salmon

shinyjscode = "
shinyjs.init = function() {
  $(window).resize(shinyjs.calcWidth);
}
shinyjs.calcWidth = function() { 
  Shiny.onInputChange('plotWidth', $(window).width());
}
"


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

Plot_Map = function(df, basemap){ 
  
  # suppresses coordinate system message
  map = suppressMessages(
    ggmap(basemap) +
    geom_sf(data = df |> filter(!rec),
            color = nonrec_color, size = 5,
            inherit.aes = FALSE
            ) +
    geom_sf(data = df |> filter(rec),
            color = rec_color, size = 5,
            inherit.aes = FALSE
            ) +
    geom_sf(data = df |> filter(rec),
            shape = 1, color = 'black', size = 5.5,
            inherit.aes = FALSE) +
    theme_void() + 
    theme(aspect.ratio = 1)
    )
  return(map)
}


set_shiny_plot_height_with_respects_to_width = function(session, output_width_name){
  width <- function() { 
    session$clientData[[output_width_name]] 
  }
  # Do something with the width
  width / 2
}



# ui ------------------------------------------------------------------------------------------
ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = shinyjscode, functions = c('calcWidth')),
    fluidRow(
      column(6, offset = 3,
             align = 'center',
             style = glue('font-family: Cooper Black; background-color:{rec_color}'),
             h1("Take-a-Hike™️")
            )
    ),
    fluidRow(
      column(6, offset = 3,
             style = glue('font-family: Cooper Black; background-color:{rec_color}'),
             fluidRow(
               column(6, align = 'left',
                      textInput("starting_location",
                                "Enter a starting location",
                                value = '',
                                width = '100%')
               ), 
               column(6,
                      sliderInput("difficulty_choices",
                                  "Hike Difficulty",
                                  min = 1,
                                  max = 7,
                                  step = 2, 
                                  value = 5,
                                  ticks = FALSE,
                                  width = '100%')
               )
             )
           )
    ),
    fluidRow(
      column(6, offset = 3,
             style = glue('font-family: Cooper Black; background-color:{rec_color}'),
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
               column(6,
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
             style = glue('font-family: Cooper Black; background-color:{rec_color}'),
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
               column(6,
                      sliderInput("max_elevation",
                                  "Elevation Gain (ft): ",
                                  min = 1,
                                  max = 10000,
                                  value = 2000,
                                  width = '100%', 
                                  ticks = FALSE)
                    )
               )
           )
  ),

# plots ---------------------------------------------------------------------------------------
  fluidRow(
    column(10, offset = 1,
           fluidRow(
             column(6, align = 'left',
                    style = glue("background-color:{nonrec_color}"),
                    uiOutput('all_trail_hover_info')
             ),
             # column(2, ''),
             column(6, align = 'left',
                    style = glue("background-color:{rec_color}"),
                    uiOutput('city_map_hover_info')
                    )
           ),
           fluidRow(
            column(6, align = 'left',
                   style = glue("background-color:{rec_color}"),
                   plotOutput("all_trail_map",
                              # width = 'auto',
                              hover = hoverOpts("all_trail_hover", delay = 50, delayType = "debounce")
                             )
            ), 
            column(6,
                   style = glue("background-color:{nonrec_color}"),
                   plotOutput("city_map",
                              # width = '100%',
                              hover = hoverOpts("city_map_hover", delay = 50, delayType = "debounce")
                   )
                   )
           )
           )
    ),
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
    
    
    plotWidth = reactive({ 
      ifelse(is.null(input$plotWidth), 0, input$plotWidths)
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
          theme(legend.position = 'none') +
          theme(aspect.ratio = 1)        
        
        all_trails_plot
        
          # grob = ggplotGrob(all_trails_plot)
          # grob$respect = FALSE
          # grid.newpage()
          # grid.draw(grob)
          }
    },
    # width = plotWidth() * (10/12) * 0.5
    # width = plotWidth
    width = 500
    )
    
    output$all_trail_hover_info = renderText({
      if (nrow(all_trails_selection()) > 0) { 
          
        hover = input$all_trail_hover
        point = nearPoints(all_trails_selection() |>
                             mutate(name = str_trunc(name, 60)),
                           hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
      
        all_trails_out = all_trails() |> 
          left_join(all_trails_selection() |> select(ID, cluster_rank), by = 'ID', keep = TRUE) |> 
          mutate(rec = !is.na(ID.y)) |> 
          rename(ID = ID.x) |> 
          select(-ID.y) |> 
          group_by(distance_cluster) |> 
          tidyr::fill(cluster_rank, .direction = 'updown') |>
          ungroup() |> 
          filter(cluster_rank == min(cluster_rank, na.rm = TRUE))
        
        city_basemap = Get_Basemap(all_trails_out,
                                   box_zoom = 1.5, 
                                   map_zoom = 11)
        
        map = Plot_Map(all_trails_out |> st_as_sf(coords = c('long', 'lat'), crs = 4326),
                 city_basemap)
        
        map
        
        # grob = ggplotGrob(map)
        # grob$respect = FALSE
        # grid.newpage()
        # grid.draw(grob)        
      }
    },
    # width = plotWidth() * (10/12) * 0.5
    # width = plotWidth
    width = 500
    )

    output$city_map_hover_info = renderText({
      if (nrow(all_trails_selection()) > 0) { 
        
        hover2 = input$city_map_hover
        point = nearPoints(all_trails() |> 
                             left_join(all_trails_selection() |> select(ID, cluster_rank), by = 'ID', keep = TRUE) |> 
                             mutate(rec = !is.na(ID.y)) |> 
                             rename(ID = ID.x) |> 
                             select(-ID.y) |> 
                             group_by(distance_cluster) |> 
                             tidyr::fill(cluster_rank, .direction = 'updown') |>
                             ungroup() |> 
                             filter(cluster_rank == min(cluster_rank, na.rm = TRUE)) |> 
                             rename(lon = long) |>
                             mutate(name = str_trunc(name, 60)) |> 
                             st_as_sf(coords = c('lon', 'lat'), crs = 4326, remove = FALSE),
                           hover2, threshold = 5, maxpoints = 1, addDist = TRUE)
        placeholder = HTML(paste0(rep('<br/>', times = 2), sep = ''))
        
        if (nrow(point) == 0) {
          placeholder
        } else {
          HTML(paste0("<b> ", point$name, "</b>", "<br/>",
                      "<b>",  round(point$length, 2), " mi </b>️" ,emo::ji('right_arrow'),
                      " <b>", round(point$elevation_gain, 0), " ft </b>️", emo::ji('up_arrow'),
                      " <b>", round(point$avg_grade, 2), "% </b>", emo::ji('up_right_arrow')))
        }
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
          rename(difficulty = difficulty_rating,
                 `hike time` = duration_minutes) |> 
          relocate(location, .after = 'link')

        DT::datatable(tbl, escape = FALSE,
                      options = list(
                        autoWidth = TRUE
                        )
                      )
        }
    })
}
# 
# input = list(max_distance = 150,
#              max_length = 7.5,
#              max_elevation = 2000,
#              max_duration = 60*4)


shinyApp(ui = ui, server = server)
