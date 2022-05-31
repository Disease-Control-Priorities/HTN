#####################################################################################################################################
# House keeping
rm(list = ls(all = TRUE))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, lubridate, tidyr, dplyr, countrycode, ggplot2, tm, mgcv, httr,
               prophet, tsibble, fable.prophet, fable, wktmo)

select    <- dplyr::select

#################################################################################################
# WPP inputs
#################################################################################################

get.wpp.adj <- function(){
load("WPP_input_data.Rda")

wpp <- wpp.input %>% filter(year_id %in% c(2019, 2020, 2021)) %>%
  rename(sex = sex_name, year = year_id) %>% 
  mutate(age = ifelse(age >= 95, 95, age), dx = mx*Nx) %>%
  group_by(location_name, iso3, sex, age, year) %>%
  summarise(dx = sum(dx), Nx = sum(Nx), .groups = "drop") %>% ungroup() %>%
  arrange(year, iso3, sex, age) %>%
  left_join(data.frame(ageg = c(rep(seq(0,90,5), each = 5), 90), age = 0:95), by = "age") %>%
  group_by(year, iso3, sex, ageg) %>%
  mutate(tdx = sum(dx)) %>% ungroup() %>% mutate(pdx = dx/tdx, tdx = NULL)

#################################################################################################
# Data by age and sex from Eurostat
#################################################################################################


eurostat <- fread("https://www.dropbox.com/s/w4pqb3b35tvnpos/eurostat.csv?dl=1")  %>%
  separate(week,c("year","week"), "W") %>%
  right_join(data.frame(ageg = c("Less than 5 years",
                                 paste0("From ", seq(5,85,5)," to ",seq(5,85,5)+4, " years"), "90 years or over", "Total"),
                        age = c(seq(0,85,5), 90, 999)), by = "ageg") %>%
  filter(!is.na(sex) & val != ":" & age != 999 & !is.na(val)) %>% 
  mutate(val = as.numeric(val), 
         sex = ifelse(sex=="Females","Female","Male"),
         iso3 = countrycode::countrycode(country, "country.name", "iso3c"),
         year = as.numeric(year)) %>%
  group_by(iso3, year, age, sex) %>%
  summarize(val  = sum(val, na.rm = T), .groups = "drop") %>% ungroup() %>%
  filter(year %in% c(2019, 2020, 2021) & iso3 %in% unique(wpp$iso3)) %>% rename(ageg = age)

### Proportional distribution of Eurostat excess
#1) Use countries with overall positive excess 
#2) assume only in ages over 30

excess.dist <- eurostat %>% 
  left_join(wpp %>% select(iso3, year, sex, ageg, age, pdx), by = c("iso3", "year", "sex", "ageg")) %>% 
  mutate(ageg = NULL, val = val*pdx, pdx = NULL) %>% 
  spread(year, val) %>%
  mutate(`2020` = `2020` - `2019`, `2021` = `2021` - `2019`, `2019` = NULL) %>% 
  group_by(iso3) %>%
  mutate(t2020 = sum(`2020`), t2021 = sum(`2021`)) %>% ungroup() %>%
  filter(!is.na(t2020) & !is.na(t2021) & t2020 > 0 & t2021 > 0) %>% 
  mutate(`2020` = ifelse(age < 30 | `2020` < 0, 0, `2020`),
         `2021` = ifelse(age < 30 | `2021` < 0, 0, `2021`)) %>% 
  group_by(age, sex) %>% 
  summarise(`2020` = sum(`2020`), `2021` = sum(`2021`), .groups = "drop") %>% ungroup() %>%
  mutate(`2020` = `2020`/sum(`2020`), `2021` = `2021`/sum(`2021`)) %>% 
  gather(year, prop.excess, -age, -sex) %>% mutate(year = as.numeric(year)) %>% 
  arrange(year, sex, age) 
         
         
######################################################################################################################
# Read in the WHO data and clean up some of the names
######################################################################################################################

ccodes         <- fread("https://www.dropbox.com/s/hoefrsvbk3lz389/ccodes.csv?dl=1")

who.covid      <- fread("https://covid19.who.int/WHO-COVID-19-global-data.csv") %>%
  filter(Country != "Other") %>%
  mutate(Country_code = ifelse(Country == "Namibia", "NA", Country_code)) %>%
  left_join(ccodes, by = "Country_code") %>%
  mutate(Country = case_when(Country == "Curaçao" ~ "Curacao",
                             Country == "Saint Barthélemy" ~ "Saint Barthelemy",
                             Country == "Réunion" ~ "Reunion",
                             Country == "Kosovo[1]" ~ "Kosovo",
                             Country %in% c("Bonaire","Saba","Sint Eustatius") ~ "Bonaire, Sint Eustatius and Saba",
                             TRUE ~ as.character(Country)),
         iso3    = case_when(Country == "Kosovo" ~ "XKX",
                             Country == "Saint Martin"~ "MAF",
                             Country == "Sint Maarten"~ "SXM",
                             Country == "Bonaire, Sint Eustatius and Saba" ~ "BES",
                             Country == "Curacao" ~ "CUW",
                             Country == "Saint Barthelemy" ~ "BLM",
                             Country == "South Sudan" ~ "SSD",
                             Country == "Namibia" ~ "NAM",
                             TRUE ~ as.character(iso3)),
         date    = as.Date(Date_reported,"%y-%m-%d"),
         year   =  lubridate::year(lubridate::ymd(date))) %>%
  group_by(iso3, year) %>%
  summarise(covid_deaths = sum(New_deaths, na.rm = T), .groups = "drop") %>% ungroup() %>%
  mutate(covid_deaths = ifelse(covid_deaths < 0, 0, round(covid_deaths))) %>%
  filter(year < 2022 & iso3 %in% unique(wpp$iso3))
#################################################################################################

wpp.adj <- wpp %>% 
  select(-c(pdx, ageg)) %>%
  filter(year > 2019) %>%
  left_join(who.covid %>% 
  left_join(excess.dist, by = "year") %>%
  mutate(covid.deaths = prop.excess * covid_deaths, 
         prop.excess = NULL, covid_deaths = NULL), by = c("year", "age", "sex", "iso3")) %>% 
  mutate(mx = dx/Nx, covid.mx = covid.deaths/Nx, covid.deaths = NULL, dx = NULL)

with(wpp.adj %>% filter(iso3 == "PER" & sex == "Female" & year == 2020), 
     plot(main = "Example: PERU female 2020", age, log(mx), ylim = c(-9, 0), type = "l", col = "red"))
with(wpp.adj %>% filter(iso3 == "PER" & sex == "Female" & year == 2020), 
     lines(age, log(mx + covid.mx), ylim = c(-9, 0), col = "blue"))

return(wpp.adj)
}

wpp.adj <- get.wpp.adj()
save(wpp.adj, file = "wpp.adj.Rda")
