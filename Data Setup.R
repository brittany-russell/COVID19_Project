# Coronavirus Project
# Brittany Russell
# Sep 10, 2020

#########################
# Data Notes:
# JHU global data is
# available from
# library(coronavirus)
#########################
# cases and deaths are cumulative, 
  # but errors lead to decreases in
  # cumulative numbers at times?
#########################
# Attribute the data as the 
# "COVID-19 Data Repository 
# by the Center for Systems Science and Engineering (CSSE) 
# at Johns Hopkins University" 
# or "JHU CSSE COVID-19 Data" for short, 
# and the url: https://github.com/CSSEGISandData/COVID-19.
#########################

library(tidyverse) # dplyr and readr
library(ggplot2) # plotting
library(magrittr) # for %<>%
library(zoo) # rollmean() for rolling avg

# Data from CSSEGISandData (Johns Hopkins University)
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
confirmed_long <- confirmed %>% gather("date", "cases", 12:ncol(confirmed))
confirmed_long$date <- parse_date(confirmed_long$date, format = "%m/%d/%y")

deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
deaths_long <- deaths %>% gather("date", "deaths", 13:ncol(deaths))
deaths_long$date <- parse_date(deaths_long$date, format = "%m/%d/%y")

# combined data set
long_dat <- deaths_long
long_dat$cases <- confirmed_long$cases
# remove all data with population of zero -- that doesn't seem useful
long_dat %<>% filter(Population != 0) # removes approx 47,000 obs
# per capita deaths and cases (per 1000)
long_dat %<>% mutate(cases_pc = 1000 * cases/Population, deaths_pc = 1000 * deaths/Population)
# time variable
long_dat %<>% mutate(days = date - min(date))
  # might need to turn this numeric? now it's difftime

# More dependent vars
long_dat %<>% arrange(Combined_Key)
long_dat %<>% group_by(Combined_Key) %>% 
  mutate(daily_cases = cases - lag(cases), daily_deaths = deaths - lag(deaths)) %>%
  mutate(daily_cases_pc = 1000 * daily_cases/Population, daily_deaths_pc = 1000 * daily_deaths/Population) %>%
  mutate(daily_cases_change = daily_cases - lag(daily_cases), daily_deaths_change = daily_deaths - lag(daily_deaths)) %>%
  mutate(daily_cases_change_pc = 1000 * daily_cases_change/Population, daily_deaths_change_pc = 1000 * daily_deaths_change/Population)
# daily_cases/deaths gives us the change in the cumulative counts by day (first derivative) 
  # -- How are the totals changing? i.e. new cases/deaths
# daily_cases/deaths/change gives us the change in the daily change (second derivative) 
  # -- How is the change in the totals changing? i.e. new cases/deaths lower than yesterday => increasing at a decreasing rate

long_dat %<>% add_column("Type" = "County")

# State data set (add up numbers across state)
state_dat <- long_dat %>% group_by(Province_State, date) %>% 
  summarize(cases = sum(cases), deaths = sum(deaths), Population = sum(Population)) %>%
  mutate(cases_pc = 1000 * cases/Population, deaths_pc = 1000 * deaths/Population) %>%
  mutate(daily_cases = cases - lag(cases), daily_deaths = deaths - lag(deaths)) %>%
  mutate(daily_cases_pc = 1000 * daily_cases/Population, daily_deaths_pc = 1000 * daily_deaths/Population) %>%
  mutate(daily_cases_change = daily_cases - lag(daily_cases), daily_deaths_change = daily_deaths - lag(daily_deaths)) %>%
  mutate(daily_cases_change_pc = 1000 * daily_cases_change/Population, daily_deaths_change_pc = 1000 * daily_deaths_change/Population) %>%
  add_column("Type" = "State")

# National numbers
nation_dat <- long_dat %>% group_by(date) %>% 
  summarize(cases = sum(cases), deaths = sum(deaths), Population = sum(Population)) %>%
  mutate(cases_pc = 1000 * cases/Population, deaths_pc = 1000 * deaths/Population) %>%
  mutate(daily_cases = cases - lag(cases), daily_deaths = deaths - lag(deaths)) %>%
  mutate(daily_cases_pc = 1000 * daily_cases/Population, daily_deaths_pc = 1000 *daily_deaths/Population) %>%
  mutate(daily_cases_change = daily_cases - lag(daily_cases), daily_deaths_change = daily_deaths - lag(daily_deaths)) %>%
  mutate(daily_cases_change_pc = 1000 * daily_cases_change/Population, daily_deaths_change_pc = 1000 * daily_deaths_change/Population) %>%
  add_column("Type" = "Nation")

## Daily cases and daily change are really jagged -- 7-day rolling avg is probably better visually
long_dat %<>% mutate(rolling_daily_cases = rollmean(daily_cases_pc, k = 7, fill = NA), rolling_daily_deaths = rollmean(daily_deaths_pc, k = 7, fill = NA))
state_dat %<>% mutate(rolling_daily_cases = rollmean(daily_cases_pc, k = 7, fill = NA), rolling_daily_deaths = rollmean(daily_deaths_pc, k = 7, fill = NA))
nation_dat %<>% mutate(rolling_daily_cases = rollmean(daily_cases_pc, k = 7, fill = NA), rolling_daily_deaths = rollmean(daily_deaths_pc, k = 7, fill = NA))

# Plotting national and state together
all_dat <- bind_rows(long_dat, state_dat, nation_dat)
all_dat$Type <- factor(all_dat$Type, levels = c("Nation", "State", "County"))

all_dat %>% filter(Type == "Nation" | (Type == "State" & Province_State == "Texas") | (Type == "County" & Combined_Key == "Bell, Texas, US")) %>%
  ggplot(mapping = aes(x = date, y = rolling_daily_cases, group = Type)) + 
  geom_line(aes(col = Type)) + 
  ylab("Cumulative Cases per 1000") +
  xlab("Time")
                   
# Export plotting dataframe to subfolder of shiny app directory
save(all_dat, file = "coronavirus_plot_app/plot_data/all_dat.RData")

# EDA (for Kansas mask orders)
long_dat %>% filter(Province_State == "Kansas") %>% ggplot(mapping = aes(x = date, y = cases_pc, group = Combined_Key)) + geom_line(aes(col = Combined_Key), show.legend = FALSE)
long_dat %>% filter(Province_State == "Kansas") %>% ggplot(mapping = aes(x = date, y = deaths_daily, group = Combined_Key)) + geom_line(aes(col = Combined_Key), show.legend = FALSE)

# Holiday effects
# Thanksgiving: 2020-11-26






