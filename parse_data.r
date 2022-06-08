library(tidyverse)
library(data.table)
library(jsonlite)
library(glue)

# pull data -----------------------------------------------------------------------------------
Get_Data = function(fname) { 
  result = fromJSON(fname)[['hits']] |>
    group_by(row_number()) |> 
    mutate(backpacking = 'backpacking' %in% unlist(activities)) |> 
    mutate(camping = 'camping' %in% unlist(activities)) |> 
    mutate(water_stuff = str_detect(activities |> paste0(), 'sea-kayaking|kayaking|paddle-sports|canoeing')) |>
    mutate(lat = `_geoloc`[[1]]) |> 
    mutate(long = `_geoloc`[[2]]) |> 
    select(-activities, -`_geoloc`)
  return(result)
}


all_states = list.files('results/', full.names = TRUE) %>% 
  .[str_detect(., '\\.json')]

all_trails = map(all_states, Get_Data) |> rbindlist(fill = TRUE)

# remove nonstarters --------------------------------------------------------------------------




# identify clusters for daytrips based on lat long  -------------------------------------------


