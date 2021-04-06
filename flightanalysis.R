library(tidyverse)
library(lubridate)
library(nycflights13)

head(flights)
flights %>%
  mutate(long_flight = (air_time >= 6 * 60)) %>%
  View()
flights %>%
  mutate(long_flight = (air_time >= 6 * 60)) %>%
  count(long_flight)


flights %>%
  group_by(date = make_date(year, month, day)) %>%
  summarise(flights_n = n(), air_time_mean = mean(air_time, na.rm = TRUE)) %>%
  ungroup()
flights %>%
  slice_sample(n = 15)

flights %>%
  slice_sample(prop = 0.15)
flights %>%
  select(year, month, day) %>%
  mutate(date = make_date(year, month, day))
numbers_1 <- tibble(number = c("#1", "Number8", "How are you 3"))
numbers_1 %>% mutate(number = parse_number(number))

flights %>%
  select(starts_with("dep_"))
flights %>%
  select(ends_with("hour"))
flights %>%
  select(contains("hour"))

flights %>%
  mutate(origin = case_when(
    (origin == "EWR") & dep_delay > 20 ~ "Newark International Airport - DELAYED",
    (origin == "EWR") & dep_delay <= 20 ~ "Newark International Airport - ON TIME DEPARTURE",
  )) %>%
  count(origin)

flights %>%
  mutate(origin = str_replace_all(origin, c(
    "^EWR$" = "Newark International",    "^JFK$" = "John F. Kennedy International"
  ))) %>%
  count(origin)

# filtering groups 
flights_top_carriers <- flights %>%
  group_by(carrier) %>%
  filter(n() >= 10000) %>%
  ungroup()

# extract rows from the first column
beginning_with_am<- airlines %>%   
  filter(name %>% str_detect("^Am"))
beginning_with_am

# etract rows which are not matched with the second table 
flights %>%
  anti_join(beginning_with_am, by = "carrier")

airline_names <- flights %>%
  left_join(airlines, by = "carrier")
airline_names %>%
  count(name) %>%
  ggplot(aes(name, n)) +
  geom_col()
airline_names %>%
  count(name) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(name, n)) +
  geom_col()



# display counts more accurately

airline_names %>%   
  count(name) %>%   
  mutate(name = fct_reorder(name, n)) %>%   
  ggplot(aes(name, n)) +   
  geom_col() +   
  coord_flip() 


# all combinations using crossing
crossing(
  customer_channel = c("Bus", "Car"),
  customer_status = c("New", "Repeat"),
  spend_range = c("$0-$10", "$10-$20", "$20-$50", "$50+"))


# group by based on function 
summary <- function(data, col_names, na.rm = TRUE) {
  data %>%
    summarise(across({{ col_names }},
                     list(
                       min = min,
                       max = max,
                       median = median,
                       mean = mean
                     ),
                     na.rm = na.rm,
                     .names = "{col}_{fn}"
    ))
}

flights_with_airline_names <- airline_names

flights_with_airline_names %>%
  summary(c(air_time, arr_delay))
flights_with_airline_names %>%
  group_by(carrier) %>%
  summary(c(air_time, arr_delay))







