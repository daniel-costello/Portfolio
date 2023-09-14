# Dan Costello
# Final Assignment

library(magrittr)
library(tidyverse)
library(janitor)
library(lubridate)
library(ggrepel)
library(scales)
library(janitor)
library(dplyr)
library(stringr)
library(readr)

# Question 1

# (a)
fcc <- read_csv("fcc_complaints_CA-2021.csv") %>% clean_names()

# (b)

fcc_clean <- fcc %>% drop_na(method) %>% filter(zip != "00000")

# (c)

fcc_clean <- fcc_clean %>% tidyr::extract(caller_id_number,
                                          into = "area_code",
                                          "(^.+?)-",
                                          remove = F)

# (d)
fcc_clean <- fcc_clean %>% mutate(month_of_issue = month(date_of_issue),
                                  year_of_issue = year(date_of_issue))

# (e)
fcc_clean <- fcc_clean %>% 
  mutate(time_of_issue_clean = str_replace_all(time_of_issue,"[.]","") %>%
           parse_time(format = "%H:%M %p"))
# (f)
call_distribution <- fcc_clean %>%
  filter(form == "Phone") %>%
  ggplot(aes(time_of_issue_clean, fill = method)) + ## edit the fill
  geom_histogram(alpha = 0.8) +
  theme_minimal() +
  scale_x_time(breaks = scales::date_breaks("3 hour"),
               labels = scales::time_format("%H:%M")) + ## edit the time_format
  theme(legend.position = "bottom") + ## edit this line
  labs(x = "", y = "Number of FCC Complaints", fill = "",
       title = "Distribution of Phone Complaints to the FCC",
       subtitle = "From January 2021 - July 2022") +
  ggthemes::scale_fill_fivethirtyeight()


# Question 2

# (a)
acs <- read_csv("acs_data.csv")

# (b)
acs <- acs %>% tidyr::extract(name,
                              into = "zip",
                              "\\s(.*)",
                              remove = F)

# (c)
fcc_joined <- left_join(x = fcc_clean,y = acs) %>%
  filter(year_of_issue == 2021, total_pop != 0)

# (d)
fcc_joined <- fcc_joined %>% 
  mutate(low_income_zip = ifelse(median_income < mean(median_income, na.rm = T),
                                 "Below Average Median Income",
                                 "Above Average Median Income"),
         high_age_zip = ifelse(median_age > mean(median_age, na.rm = T),
                               "Above Average Median Age",
                               "Below Average Median Age"))

# (e)
aggregate <- fcc_joined %>%
  group_by(median_age, median_income, total_pop, zip, low_income_zip) %>%
  summarize(n_complaints = n())

# (f)

aggregate <- aggregate %>% mutate(complaints_per_1000 = 
                                    (n_complaints / total_pop) * 1000)