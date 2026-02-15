library(tidyverse)
library(leaflet)
library(sf)
library(ggiraph)
library(patchwork)
library(paletteer)
library(echarts4r)

data <- read_csv("listings.csv")

#EDA

class(data$price) # charachter

#date of data


dates <- unique(data$last_scraped)
# The data was scrapped between 2025-01-29 an 2025-02-02

mean(data$last_scraped == "2025-01-30") # 74%
mean(data$last_scraped == "2025-01-29") # 24%
mean(data$last_scraped == "2025-01-31") # 1.5%
mean(data$last_scraped == "2025-02-02") # 0.00003 %


#Remove "$" and ",", then convert to numeric and to dollar using cotization of 01-30-2025
data1 <- data %>% 
  mutate(price = str_remove(price, "\\$")) %>% 
  mutate(price = str_replace_all(price, ",", "")) %>% 
  mutate(price = as.numeric(price)/1032,
         revenue = estimated_revenue_l365d/1032)
  
#obtain summary of price for each neighbourhood

obtain_summary <- function(a){
  subseted <- data1 %>% filter(neighbourhood_cleansed == a)
  summary(subseted$price)
}

neighnourhoods <- data1 %>% 
  select(neighbourhood_cleansed) %>% 
  pull() %>% 
  unique()

prices <- sapply(neighnourhoods, obtain_summary)

#obtain summary of review scores

# Calculate price median for each neighbourhood

data2 <- data1 %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(median_price = median(price, na.rm = TRUE), 
            median_revenue = median(revenue, na.rm = TRUE),
            median_occupancy = median(estimated_occupancy_l365d, na.rm = TRUE),
            median_reviews = median(reviews_per_month, na.rm = TRUE),
            n = n()) %>% 
  arrange(desc(median_price))

data2 %>% 
  ggplot(aes(median_price, reorder(neighbourhood_cleansed, median_price)))+
  geom_col()+
  geom_text(aes(label = paste("n = ", n)), hjust = -0.05)+
  xlab("Median Price per Night (US dollars)")+
  ylab("")+
  theme_minimal()



data2 %>% 
  ggplot(aes(median_price, reorder(neighbourhood_cleansed, median_price)))+
  geom_point()+
  geom_segment(aes(x = 0, xend = median_price, y = reorder(neighbourhood_cleansed, median_price), yend = reorder(neighbourhood_cleansed, median_price)))+
  geom_text(aes(label = paste("n = ", n)), hjust = -0.1)


# Obtain price range 

data3 <- data1 %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(q1 = quantile(price, na.rm = TRUE)[2],
            q3 = quantile(price, na.rm = TRUE)[4]) %>% 
  arrange(desc(q3))

g <- data3 %>% 
  ggplot()+
  geom_segment(aes(x = q1, xend = q3, y = reorder(neighbourhood_cleansed, q3), yend = reorder(neighbourhood_cleansed, q3)))+
  geom_point(aes(q3, y = reorder(neighbourhood_cleansed, q3)))+
  geom_point(aes(q1, y = reorder(neighbourhood_cleansed, q3)))+
  scale_x_continuous(limits = c(0,150))+
  xlab("Price range per night (US dollars)")+
  ylab("")+
  theme_minimal()


               




# map PRICE

sf <- jsonlite::read_json("caba_barrios.geojson")

sf2 <- read_sf("caba_barrios.geojson")

data2 <- data2 %>% 
  mutate(neighbourhood_cleansed = str_to_upper(neighbourhood_cleansed)) %>% 
  rename("BARRIO" = "neighbourhood_cleansed")

data2 %>% 
  e_chart(x = BARRIO) %>% 
  e_bar(serie = median_price ) %>% 
  e_flip_coords()

# data_map <- left_join(sf, data2, by = c("BARRIO" = "neighbourhood_cleansed"))
# 
# map <- data_map %>% 
#   ggplot() +
#   geom_sf_interactive(aes(fill = median_price, data_id = BARRIO, tooltip = paste(BARRIO, "\n",
#                                                                                  "Median Price: $", round(median_price,0)))) +
#   theme_void()+
#   labs(fill = "Median Price per Night (US Dollars)")+
#   scale_fill_paletteer_c("grDevices::Viridis", 1)
#   theme(
#         axis.title = element_blank())
#   
# girafe(ggobj = map)
# 
# 
# #MAP REVENUE
# 
# 
# 
# map2 <- data_map %>% 
#   ggplot() +
#   geom_sf_interactive(aes(fill = median_revenue, data_id = BARRIO, tooltip = paste(BARRIO, "\n",
#                                                                                    "Median Yearly Revenue: $", round(median_revenue,0)))) +
#   theme_void()+
#   labs(fill = "Median yearly revenue (US Dollars)")+
#   scale_fill_paletteer_c("grDevices::Viridis", 1)
# theme(
#   axis.title = element_blank())
# 
# girafe(ggobj = map2)
# 
# 
# # map number of listings
# 
# map3 <- data_map %>% 
#   ggplot() +
#   geom_sf_interactive(aes(fill = n, data_id = BARRIO, tooltip = paste(BARRIO, "\n", 
#                                                                       "Number of Listings:", n))) +
#   theme_void()+
#   labs(fill = "Number of Listings")+
#   scale_fill_paletteer_c("grDevices::Viridis", 1)
# theme(
#   axis.title = element_blank())
# 
# girafe(ggobj = map3)
# 
# # Map occupancy
# 
# map4 <- data_map %>% 
#   ggplot() +
#   geom_sf_interactive(aes(fill = median_occupancy, data_id = BARRIO, tooltip = paste(BARRIO, "\n", 
#                                                                       "Median Occupancy:", median_occupancy, " days"))) +
#   theme_void()+
#   labs(fill = "Median Yearly Occupancy (days)")+
#   scale_fill_paletteer_c("grDevices::Viridis", 1)
# theme(
#   axis.title = element_blank())
# 
# girafe(ggobj = map4)
# 
# # Review score value
# 
# map5 <- data_map %>% 
#   ggplot() +
#   geom_sf_interactive(aes(fill = median_reviews, data_id = BARRIO, tooltip = paste(BARRIO, "\n", 
#                                                                                      "Median Reviews per Month:", median_reviews))) +
#   theme_void()+
#   labs(fill = "Median Reviews per Month")+
#   scale_fill_paletteer_c("grDevices::Viridis", 1)
# theme(
#   axis.title = element_blank())
# 
# girafe(ggobj = map5)
# 
# combined <- map / map2 + plot_layout(ncol = 1)
# 
# girafe(ggobj = combined)
# 
# combined2 <- map3 / map4 + plot_layout(ncol = 1)

# girafe(ggobj = combined2)



c1 <- data2 %>% 
  e_charts(BARRIO) %>% 
  e_map_register("CABA", sf) %>% 
  e_map(serie = median_price, map = "CABA", nameProperty = "BARRIO") %>% 
  e_visual_map(median_price) %>% 
  e_theme("roma") %>% 
  e_title("Puerto Madero and the north side of the city have the highest median rent prices (US$)")


c2 <- data2 %>% 
  e_charts(BARRIO) %>% 
  e_map_register("CABA", sf) %>% 
  e_map(serie = median_revenue, map = "CABA", nameProperty = "BARRIO") %>% 
  e_visual_map(median_revenue) %>% 
  e_theme("green") %>% 
  e_title("Puerto Madero and the north side of the city have the highest rent prices")

e_arrange(c1, c2, rows = 2)


data1 %>% 
  filter(price>16 & price<137) %>% 
  mutate(price = round(price,0)) %>% 
  e_charts(longitude) %>% 
  e_leaflet(
    center = c(-58.4, -34.6),
    zoom = 12) %>% 
  e_leaflet_tile() %>% 
  e_scatter(latitude,
            size = price,
            coord_system = "leaflet",
            bind = listing_url) %>%
  e_add_unnested("rating", review_scores_rating) %>% 
  e_tooltip(formatter = htmlwidgets::JS("
      function(params){
      let rating = params.data.rating ? params.data.rating : 'N/A';
        return(`<div style='padding:5px;'>
        <a href = '${params.name}' target='_blank' style='font-weight:bold; color:#FF5A5F;'><strong>See in Airbnb</strong></a><br/>
        Price: ${params.value[2]}<br/>
        Rating : ${rating}\u2B50 </div>`)
      }
    "), trigger = "item",
            enterable = TRUE) %>% 
  e_legend(show = FALSE) 




data1 %>% 
  filter(price > 16 & price < 137) %>% 
  mutate(price = round(price, 0)) %>% 
  # 1. Corregido: es e_charts(), no e_chart()
  e_charts(longitude) %>% 
  e_leaflet(
    center = c(-58.4, -34.6),
    zoom = 12
  ) %>% 
  e_leaflet_tile() %>% 
  e_scatter(
    latitude,
    size = price,
    coord_system = "leaflet",
    bind = listing_url # Esto guarda la URL en params.name
  ) %>%
  # 2. Fundamental: pasamos el rating para que el tooltip lo reconozca
  e_add_nested("rating", review_scores_rating) %>% 
  e_tooltip(
    formatter = htmlwidgets::JS("
      function(params){
        // params.name contiene el 'listing_url' por el bind anterior
        // params.value[2] contiene el 'price' por ser la variable de tamaño
        // params.data.rating viene de e_add_nested
        
        let rating = params.data.rating ? params.data.rating : 'N/A';
        
        return(`
          <div style='padding:5px;'>
            <a href='${params.name}' target='_blank' style='font-weight:bold; color:#FF5A5F;'>Ver en Airbnb</a><br/>
            <b>Precio:</b> $${params.value[2]}<br/>
            <b>Rating:</b> ${params.data.rating}     </div>
        `);
      }
    "), 
    trigger = "item",
    enterable = TRUE # Permite mover el mouse sobre el tooltip para hacer click en el link
  ) %>% 
  e_legend(show = FALSE)