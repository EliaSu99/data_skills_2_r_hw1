# Question 1
setwd("~/Desktop/data_skills_2_r_hw1")

library(tidyverse)
library(lubridate)

industry <- read.csv("SAEMP25N by industry.csv", skip = 4, na = c("(T)", "(D)"))
total <- read_csv("SAEMP25N total.csv", skip = 4)

trim.leading <- function (x)  sub("^\\s+", "", x)

industry <- industry %>%
  filter(GeoName != "", Description != "By industry") %>%
  select(-c("GeoFips", "LineCode")) %>%
  rename("State" = "GeoName")
  
industry$Description <- trim.leading(industry$Description)
  
total <- total %>%
  filter(GeoName != "") %>%
  select(-c("GeoFips")) %>%
  rename( "State" = "GeoName", "Total2000" = "2000", "Total2017" = "2017")

merged <-left_join(industry, total)

merged <- merged %>%
  transmute(
    State,
    Description,
    "2000" = X2000 / Total2000,
    "2017" = X2017 / Total2017
  )
  
reshaped1 <- pivot_longer(merged, !c(State, Description), names_to = "Year", values_to = "count")

reshaped2 <- pivot_wider(reshaped1, names_from = Description, values_from = count)

write.csv(reshaped2,"~/Desktop//data_skills_2_r_hw1/data.csv", row.names = FALSE)

# Question 2
## a
reshaped2 %>%
  arrange(desc(Manufacturing)) %>%
  head(5)

## Indiana, Wisconsin, Michigan, Arkansas, North Carolina

reshaped2 %>%
  filter(State %in% c("Indiana", "Wisconsin", "Michigan", "Arkansas", "North Carolina")) %>%
  ggplot(aes(fill = Year, x = State, y = Manufacturing)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y = "Share of Manufacturing Employment",
       title = "How Share of Employment Changed in Manufacturing between 2000 and 2017")
  
## b

reshaped2 %>%
  filter(Year == 2000) %>%
  gather(Industry, Share, 3:12) %>% 
  group_by(State) %>% 
  slice(which.max(Share)) %>%
  arrange(desc(Share)) %>%
  head(5) %>%
  select(State, Industry)


reshaped2 %>%
  filter(Year == 2017) %>%
  gather(Industry, Share, 3:12) %>% 
  group_by(State) %>% 
  slice(which.max(Share)) %>%
  arrange(desc(Share)) %>%
  head(5) %>%
  select(State, Industry)

## District of Columbia, Alaska, Hawaii, New Mexico, and Wyoming have the the highest concentration 
## of employment in a any single industry in either 2000 or 2017. And the most concentrated industry 
## is Government and government enterprises for all the five states.
         