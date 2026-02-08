library(tidyverse)
library(leaflet)
library(sf)
library(ggiraph)
library(patchwork)
library(paletteer)

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

sf <- read_sf("caba_barrios.geojson")

data2 <- data2 %>% 
  mutate(neighbourhood_cleansed = str_to_upper(neighbourhood_cleansed))

data_map <- left_join(sf, data2, by = c("BARRIO" = "neighbourhood_cleansed"))

map <- data_map %>% 
  ggplot() +
  geom_sf_interactive(aes(fill = median_price, data_id = BARRIO, tooltip = paste(BARRIO, "\n",
                                                                                 "Median Price: $", round(median_price,0)))) +
  theme_void()+
  labs(fill = "Median Price per Night (US Dollars)")+
  scale_fill_paletteer_c("grDevices::Viridis", 1)
  theme(
        axis.title = element_blank())
  
girafe(ggobj = map)


#MAP REVENUE



map2 <- data_map %>% 
  ggplot() +
  geom_sf_interactive(aes(fill = median_revenue, data_id = BARRIO, tooltip = paste(BARRIO, "\n",
                                                                                   "Median Yearly Revenue: $", round(median_revenue,0)))) +
  theme_void()+
  labs(fill = "Median yearly revenue (US Dollars)")+
  scale_fill_paletteer_c("grDevices::Viridis", 1)
theme(
  axis.title = element_blank())

girafe(ggobj = map2)


# map number of listings

map3 <- data_map %>% 
  ggplot() +
  geom_sf_interactive(aes(fill = n, data_id = BARRIO, tooltip = paste(BARRIO, "\n", 
                                                                      "Number of Listings:", n))) +
  theme_void()+
  labs(fill = "Number of Listings")+
  scale_fill_paletteer_c("grDevices::Viridis", 1)
theme(
  axis.title = element_blank())

girafe(ggobj = map3)

# Map occupancy

map4 <- data_map %>% 
  ggplot() +
  geom_sf_interactive(aes(fill = median_occupancy, data_id = BARRIO, tooltip = paste(BARRIO, "\n", 
                                                                      "Median Occupancy:", median_occupancy, " days"))) +
  theme_void()+
  labs(fill = "Median Yearly Occupancy (days)")+
  scale_fill_paletteer_c("grDevices::Viridis", 1)
theme(
  axis.title = element_blank())

girafe(ggobj = map4)

# Review score value

map5 <- data_map %>% 
  ggplot() +
  geom_sf_interactive(aes(fill = median_reviews, data_id = BARRIO, tooltip = paste(BARRIO, "\n", 
                                                                                     "Median Reviews per Month:", median_reviews))) +
  theme_void()+
  labs(fill = "Median Reviews per Month")+
  scale_fill_paletteer_c("grDevices::Viridis", 1)
theme(
  axis.title = element_blank())

girafe(ggobj = map5)