
box::use(
    readr[read_csv]
  , dplyr[mutate, filter, select, distinct, add_row, bind_rows, rename]
  , magrittr[`%>%`]
  , tidyr[tribble]
  , urbnmapr[...]
  , logger[...]
)


#############################
# shape data for long/lat borders 

shp_state <- urbnmapr::get_urbn_map(map = "territories_states"  , sf = FALSE) 
shp_terr_only <- filter(shp_state, !state_abbv %in% c(datasets::state.abb, "DC"))
shp_cnty_PR <- filter(urbnmapr::get_urbn_map(map = "territories_counties", sf = FALSE), state_abbv ==  "PR")

#use whole territories, don't have county-level data for territories at this time 
shp_cnty  <- bind_rows(
    urbnmapr::get_urbn_map(map = "counties", sf = FALSE)
  , shp_terr_only
)

#############################
# Abbreviations and Names for Geographies 

# 3,147 + 3,142 counties in 50 states and DC + 5 territories   
abb_cnty  <- distinct(select(shp_cnty,  county_fips, county_name, state_abbv, state_name))

# 57 items = 50 states + 1 federal district + 5 territories + 1 National abbreviation
abb_state <- distinct(select(shp_state,                           state_abbv, state_name)) %>% 
  add_row(state_abbv = "USA", state_name = "United States")


################################
#CENSUS INFORMATION, POPULATIONS 
#https://www.census.gov/programs-surveys/popest/data/data-sets.html
# use POPEST2019 for 50 states, PR, DC (and counties) --> other 4 territories use 2020 census data 

#SOURCE STATE: 
# web page: https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html
#csv: http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv
#Link to CSV state population data  (only for 50 states, DC, and PR)
raw_state <- read_csv("./data/census_tables/nst-est2019-alldata.csv")
pop_state0 <- select( #50 States, DC, and PR = 52 items 
  filter(raw_state, SUMLEV == "010" | SUMLEV == "040")#filter if 010 = National or 040 = State or PR (remove region rows)
  , county_fips = STATE
  , state_name = NAME 
  , POP = POPESTIMATE2019
) %>% mutate(county_fips = as.numeric(county_fips))


#SOURCE CNTY
#web page: https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html
#csv: https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv
#Link to CSV county population data  (only for 50 states and DC)
raw_pop   <- read_csv("./data/census_tables/co-est2019-alldata.csv")
pop_cnty0 <- raw_pop %>%
  filter(COUNTY != "000") %>% #remove state values 
  mutate(county_fips = as.numeric(paste0(STATE, COUNTY))) %>% 
  select(county_fips, state_name = STNAME, POP = POPESTIMATE2019)


#TERRITORIES: #https://www.census.gov/programs-surveys/decennial-census/decade/2020/planning-management/release/2020-island-areas-data-products.html
#2020 Island Areas Censuses Data Products 
#Take overall Population for 2020
pop_terr <- tribble(
  ~county_fips, ~state_name, ~POP
  ,         60, "American Samoa" ,  49710
  ,         66, "Guam"           , 153836
  ,         69, "Mariana Islands",  47329
  ,         78, "Virgin Islands" ,  87146 
)


pop_state <- bind_rows(pop_state0, pop_terr) 
pop_cnty  <- bind_rows(pop_cnty0 , pop_terr, pop_state0[which(pop_state0$state_name == "Puerto Rico"),]) 


#########################
## HEX MAP SHAPE FILES 

shp_hex         <- read_csv("./data/hex.csv") %>%
  rename(state_name = name) %>%
  mutate(state_name = ifelse(state_name == "Northern Mariana Islands", "Mariana Islands", state_name))
shp_hex_centers <- read_csv("./data/hex_centers.csv") %>% 
  rename(state_abb = id)  %>% 
  rename(state_name = name) %>% 
  mutate(state_name = ifelse(state_name == "Northern Mariana Islands", "Mariana Islands", state_name)) 


#' Save all data frames 
#' @export
save_static_data <- function(){
  
  saveRDS(shp_cnty , file = "./data/shp_cnty.RDS")
  log_success("Saved shp_cnty.RDS")
  
  saveRDS(pop_state, file = "./data/pop_state.RDS")
  log_success("Saved pop_state.RDS")
  saveRDS(pop_cnty , file = "./data/pop_cnty.RDS")
  log_success("Saved pop_cnty.RDS")
  
  saveRDS(shp_hex,          file = "./data/shp_hex.RDS")
  log_success("Saved shp_hex.RDS")
  saveRDS(shp_hex_centers , file = "./data/shp_hex_centers.RDS")
  log_success("Saved shp_hex_centers.RDS")
}


