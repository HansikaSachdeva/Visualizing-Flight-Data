#Loading tidyverse
library(tidyverse)

#Reading airports.csv and routes.csv from the working directory
airports <- read_csv("airports.csv", 
                     col_names = c("id", "name", "city", "country", "iata", "icao", "lat", "long",
                                   "alt", "tz", "dst", "tz2", "type", "source"), na = c("\\N"))
flights <- read_csv("routes.csv",
                    col_names = c("airline", "id", "src_airport", "src_airport_id", "dest_airport",
                                  "dest_airport_id", "codeshare", "stops", "equip"), na = c("\\N"))

airports <- airports %>% select(id, lat, long)
flights <- flights %>% select(src_airport_id, dest_airport_id) %>%
    filter(!is.na(src_airport_id) & !is.na(dest_airport_id))

#Cleaning and combining the two datasets
#Latitude and longitude data for source and destination airports
#Creating a new column for source and destination airports, and removing duplicates
#Capturing flights from Europe separately. Otherise Europe isn't visible because of the dense air traffic
clean_flights <- flights %>%  
    left_join(airports, by = c("src_airport_id" = "id")) %>%  
    rename(src_lat=lat, src_long=long) %>%  
    left_join(airports, by = c("dest_airport_id" = "id")) %>%  
    rename(dest_lat=lat, dest_long=long)  %>%  
    filter(src_airport_id != dest_airport_id)  %>%
    mutate(ordered_pair = if_else(src_airport_id > dest_airport_id,
                                  paste0(dest_airport_id,src_airport_id),
                                  paste0(src_airport_id,dest_airport_id))) %>%
    filter(!duplicated(ordered_pair)) %>%
    mutate(is_europe = if_else(dest_lat < 60 & dest_lat > 25 & src_lat < 60 & src_lat > 10 &
                                   dest_long < 50 & dest_long > -15 & src_long < 50 & src_long > -15, TRUE,FALSE)) %>%
    filter(!is.na(is_europe))

#Visualizing the data
data <- map_data("world")

data %>% filter(`region` != "Antarctica") %>%
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(fill="black", color = "white", size=0.15) +
    geom_curve(data = clean_flights, aes(x = src_long, xend = dest_long,
                                         y = src_lat,  yend = dest_lat,
                                         size=is_europe), alpha=0.07,
               color = "light blue", inherit.aes = FALSE) +
    scale_size_manual(values = c(0.05, 0.01) ) +
    theme_void() +
    theme(plot.background=element_rect(fill="black"), legend.position="none") +
    coord_equal()
