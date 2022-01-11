
#doesn't work in box::use({package}[...]) 
#works when use {ggplot2}, i.e. box::use(ggplot2[...])
#don't know why ? when use function use {package}::function()
# box::use(
#     lubridate[mdy]
#   , data.table[...]
#   , logger[...]
# )


###########################
#links for pulling down data 

#JOHN HOPKINS GITHUB 
#SOURCE: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series

#' Link to CSV cases data from John Hopkins 
link_jh_cases  <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"

#' Link to CSV deaths data from John Hopkins 
link_jh_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"

#OUR WORLD IN DATA GITHUB
#SOURCE: https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations
# data pulled from CDC API: https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_data
# CDC webpage https://covid.cdc.gov/covid-data-tracker/#vaccinations

#' Link to CSV vaccines data from Our World in Data (OWID) 
link_owid_vac <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv"


#####################################
#' Pull Raw Data from JH
#' @param LINK Character(1) - character vector of length 1 
#' @param TYPE Character(1) - character vector of length 1 
#' @return Long DF where each date and cumulative county are on rows 
#' @examples pull_JH(link_jh_cases,  "C")
#' @examples pull_JH(link_jh_deaths, "D")
pull_JH <- function(LINK , TYPE){
  # LOAD IN DATA 
  #DT00 <- data.table::fread(LINK) #CANNOT GET fread() to work, when push to shinyapps.io get 'error' (DON'T KNOW WHY???)
  DT00 <- vroom::vroom(LINK)
  data.table::setDT(DT00)
  
  # KEEP SPECIFIC COLUMNS AND RENAME 
  cols_to_keep <- c(which(colnames(DT00) %in% c("FIPS", "Admin2", "Province_State")), grep("/", colnames(DT00)))
  DT <- DT00[, ..cols_to_keep] 
  data.table::setnames(DT, c("FIPS", "Admin2", "Province_State"), c("county_fips", "county_name", "state_name"), skip_absent = TRUE)
  
  #convert names to match URBNMAPR data 
  data.table::setDT(DT)[, `:=` (state_name = ifelse(state_name == "Northern Mariana Islands", "Mariana Islands", state_name))]
  
  # create county_fips (this is because population is available for PR as a whole, not by county fips)
  data.table::setDT(DT)[, `:=` (county_fips = ifelse(state_name  == "Puerto Rico",            72, county_fips))]
  
  #clean up county/state names 
  data.table::setDT(DT)[, `:=` (county_name = ifelse(state_name  == "Puerto Rico", "Puerto Rico", county_name))]
  data.table::setDT(DT)[, `:=` (county_name = ifelse(county_name == ""            , state_name  , county_name))]
  
  #sum accounts over new county_fips (which will group PR values together) 
  date_cols <- grep("/", colnames(DT))
  DT <- DT[, lapply(.SD, sum), by = c("county_fips", "county_name", "state_name"), .SDcols = date_cols]
  
  #melt (pivot longer) so dates are in rows instead of column names 
  DT <- data.table::melt(DT, id.vars = c("county_fips", "county_name", "state_name"), variable.name = "DATE", value.name = paste0(TYPE, "_CUM"))
  
  #convert dates from character to numeric dates 
  data.table::setDT(DT)[, `:=` (DATE = lubridate::mdy(DATE))]
  
  return(DT)
}



####################################3
#' Return Data after pulled 
raw_data <- function(){
  logger::log_info("start JH pull")
  jh_C <- pull_JH(link_jh_cases, "C")
  logger::log_info("pull Cases from JH github")
  jh_D <- pull_JH(link_jh_deaths, "D")
  logger::log_info("pull Deaths from JH github")
  DT <- merge(jh_C, jh_D)[order(county_fips),]
  DT[order(county_fips),]
  logger::log_info("end JH pull")
  return(DT)
}


####################################
#' Return Data after pulled (Vaccine Data)
pull_owid <- function(FILTER_NAMES){
  DT00 <- vroom::vroom(link_owid_vac) 
  data.table::setDT(DT00)
  
  cols_to_keep <- c(which(colnames(DT00) %in% c("location", "date", "people_vaccinated", "people_fully_vaccinated", "daily_vaccinations_raw", "daily_vaccinations")))
  DT <- DT00[, ..cols_to_keep] 
  data.table::setnames(
      DT
    , c("location"  , "date", "people_vaccinated", "people_fully_vaccinated", "daily_vaccinations_raw", "daily_vaccinations")
    , c("state_name", "DATE", "V_pANY"           , "V_pFULL"                , "V_NEW"                 , "V_MA7")
    , skip_absent = TRUE)
  
  #convert names to match URBNMAPR data 
  data.table::setDT(DT)[, `:=` (state_name = ifelse(state_name == "Northern Mariana Islands", "Mariana Islands", state_name))]
  data.table::setDT(DT)[, `:=` (state_name = ifelse(state_name == "Republic of Palau"       , "Palau"          , state_name))]
  data.table::setDT(DT)[, `:=` (state_name = ifelse(state_name == "New York State"          , "New York"       , state_name))]
  
  #filter to only include specific geos 
  DT <- DT[which(state_name %in% FILTER_NAMES)]
  
  #filter to include only dates >= 2021-01-12 (first day of data for states)
  DT <- DT[which(DATE >= "2021-01-12")]
  
  #if NA, fill in with most recent value
  DT[, `:=`
     (   V_pANY  = zoo::na.locf(ifelse(DATE == "2021-01-12" & is.na(V_pANY ) == TRUE, 0, V_pANY ))
       , V_pFULL = zoo::na.locf(ifelse(DATE == "2021-01-12" & is.na(V_pFULL) == TRUE, 0, V_pFULL))
     ), by = state_name]
  
  return(DT)
}


##############################
# functions for state/county dt creation

#' Calculate New Cases by Taking Today's Cumulative Cases - Yesterday's Cumulative Cases
#' @param CUM 
calc_new <- function(CUM, DATE){
  startdate <- as.Date("2020-01-22")
  NEW <- CUM - data.table::shift(CUM)
  OUT <- dplyr::case_when(
    DATE == startdate           ~ as.integer(CUM) #if first date of data use Cumulative data 
    ,                     NEW < 0 ~ as.integer(0)   #if 'new' is negative (cases counts were updated) just set ot zero
    , DATE >  startdate & NEW >=0 ~ as.integer(NEW) #if asfter start and new is >=0, then use new value 
  )
  return(OUT)
}

#' Calculated Cases per 100k people 
#' @param POP 
#' @param VAL 
calc_p100K <- function(VAL, POP){
  ifelse(is.na(POP) | POP == 0, NA, VAL/POP) * 100000
}

#' Calculated Cases Per Capital (Proportion of Population)
#' @param POP 
#' @param VAL 
calc_pc <- function(VAL, POP){
  ifelse(is.na(POP) | POP == 0, NA, VAL/POP)
}


##############################
# create dt ######################

#items in JH data that's not in county population 
# - 116 places
# -   2 = Grand Princess + Diamond Princess 
# -  51 = 50 states + DC with unassigned counties 
# -  51 = 50 states + DC with 'Out of _' for counties 
# -  12 = Other 

#' Create Data Frames for App 
#' @param RAWDT 
#' @param POPDT 
#' @param LEV 
#' @examples
create_dt  <- function(RAWDT, POPDT){
  DT <- RAWDT
  
  #order 
  logger::log_info("order columns")
  DT <- DT[DT[,do.call(order, .SD), .SDcols = c("state_name", "county_fips", "DATE")]]
  
  logger::log_info("sum up by STATE level")
  DT <- DT[, lapply(.SD, sum), by = c("state_name", "DATE"), .SDcols = c("C_CUM", "D_CUM")]
  usa <- DT[, lapply(.SD, sum), by = c("DATE"), .SDcols = c("C_CUM", "D_CUM")]
  data.table::setDT(usa)[, `:=` (state_name = "United States")]
  DT <- rbind(usa, DT)
  
  logger::log_info("add vaccine data")
  DT_VAC <- pull_owid(FILTER_NAMES = unique(DT$state_name))
  max_date <- min(max(DT$DATE), max(DT_VAC$DATE)) #determine 'max date' in case cases/deaths have different date in vaccine 
  DT <- merge(DT, DT_VAC, all.x = TRUE)[which(DATE <= max_date)] 
  attr(DT, 'sorted') <- NULL #remove sorting attributes, in order to merge with population info
  
  
  #combine with population data 
  logger::log_info("merge raw dt with population data ")
  DT <- merge(DT, POPDT, all.x = TRUE)
  
  #calculate new cases 
  logger::log_info("creating NEW columns")
  data.table::setDT(DT)[, `:=`
            (   C_NEW = calc_new(C_CUM, DATE)
              , D_NEW = calc_new(D_CUM, DATE)
            ), by = county_fips]
  
  #calculate 7day moving average 
  logger::log_info("creating MA7 columns")
  data.table::setDT(DT)[, `:=`
            (   C_MA7 = c(rep(NA, 6), zoo::rollmean(C_NEW, k = 7, na.rm = TRUE))
              , D_MA7 = c(rep(NA, 6), zoo::rollmean(D_NEW, k = 7, na.rm = TRUE))
            ), by = county_fips]
  
  
  #start calculating proportional columns 
  logger::log_info("creating PROP columns")
  data.table::setDT(DT)[,`:=`
            (  
                C_CUM_PC    = calc_pc(   C_CUM, POP)
              , C_NEW_P100K = calc_p100K(C_NEW, POP)
              , C_MA7_P100K = calc_p100K(C_MA7, POP)
              , D_CUM_PC    = calc_pc(   D_CUM, POP)
              , D_NEW_P100K = calc_p100K(D_NEW, POP)
              , D_MA7_P100K = calc_p100K(D_MA7, POP)
              , D_FAT       = ifelse(C_CUM == 0, 0, (D_CUM/C_CUM))
              , V_pANY_PC   = calc_pc(   V_pANY , POP) # not available for CNTY LEVEL, WILL NEED TO CHANGE IF ADD CNTY
              , V_pFULL_PC  = calc_pc(   V_pFULL, POP) # not available for CNTY LEVEL, WILL NEED TO CHANGE IF ADD CNTY
              , V_MA7_P100K = calc_p100K(V_MA7  , POP) # not available for CNTY LEVEL, WILL NEED TO CHANGE IF ADD CNTY
              
            )]
  
  logger::log_success("complete creating DT")
  return(DT)
} #end create_dt 


#####################################

#' Pull/Create Data Tables 
get_data <- function(POPSTATE){
  rawDT     <- raw_data()
  out<- create_dt(rawDT, POPSTATE)
  return(out)
}



