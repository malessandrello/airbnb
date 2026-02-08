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


price_dist <- data1 %>% 
  ggplot(aes(price))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(trans = "log10")+
  facet_wrap(~ neighbourhood_cleansed)