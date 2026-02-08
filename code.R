library(tidyverse)

data <- read_csv("listings.csv")

#EDA

class(data$price) # charachter

#Remove $ and , then convert to numeric
data1 <- data %>% 
  mutate(price = str_remove(price, "\\$")) %>% 
  mutate(price = str_replace_all(price, ",", "")) %>% 
  mutate(price = as.numeric(price))
  
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

# Calculate price median for each neighbourhood

data2 <- data1 %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(median = median(price, na.rm = TRUE), n = n()) %>% 
  arrange(desc(median))

data2 %>% 
  ggplot(aes(median, reorder(neighbourhood_cleansed, median)))+
  geom_col()+
  geom_text(aes(label = paste("n = ", n)), hjust = -0.05)

data2 %>% 
  ggplot(aes(median, reorder(neighbourhood_cleansed, median)))+
  geom_point()+
  geom_segment(aes(x = 0, xend = median, y = reorder(neighbourhood_cleansed, median), yend = reorder(neighbourhood_cleansed, median)))+
  geom_text(aes(label = paste("n = ", n)), hjust = -0.1)


data1 %>% 
  group_by(neighbourhood_cleansed) %>% 
  mutate(median = median(price, na.rm = TRUE)) %>% 
  ggplot(aes(price, reorder(neighbourhood_cleansed, median)))+
  scale_x_continuous(trans = "log10")+
  geom_boxplot()


price_dist <- data1 %>% 
  ggplot(aes(price))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(trans = "log10")+
  facet_wrap(~ neighbourhood_cleansed)