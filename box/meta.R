
#' @export
VAROPTS <- tibble::tribble(
  ~GROUP,     ~VAR      , ~TYPE  ,  ~RNDVAL  , ~ACCURACY, ~NAME
  ## CASES ##
  ,  "C", "C_CUM"      , "count", 100       , 1      , "Total Cases"
  ,  "C", "C_NEW"      , "count", 10        , 1      , "Daily Cases"
  ,  "C", "C_MA7"      , "count", 10        , 1      , "Daily Cases 7-Day Moving Average"
  ,  "C", "C_CUM_PC"   , "pc"   , 0.001     , 0.1    , "Total Cases as Percent of Population"
  ,  "C", "C_NEW_P100K", "p100k", 1         , 1      , "Daily Cases per 100,000"
  ,  "C", "C_MA7_P100K", "p100k", 1         , 1      , "Daily Cases 7-Day Moving Average per 100,000"
  ## DEATHS ##
  ,  "D", "D_CUM"      , "count", 100       , 1      , "Total Deaths"
  ,  "D", "D_NEW"      , "count", 10        , 1      , "Daily Deaths"
  ,  "D", "D_MA7"      , "count", 10        , 1      , "Daily Deaths 7-Day Moving Average"
  ,  "D", "D_CUM_PC"   , "pc"   , 0.0001    , 0.01   , "Total Deaths as Percent of Population"
  ,  "D", "D_NEW_P100K", "p100k", 0.0001    , 0.01   , "Daily Deaths per 100,000"
  ,  "D", "D_MA7_P100K", "p100k", 0.0001    , 0.01   , "Daily Deaths 7-Day Moving Average per 100,000"
  ,  "D", "D_FAT"      , "pc"   , 0.0001    , 0.01   , "Fatality Rate (Total Deaths / Total Cases)"
  ## VACCINES ##
  ,  "V", "V_MA7_P100K", "p100k", 1         , 1      , "Daily Vaccination Doses 7-Day Moving Average per 100,000"
  ,  "V", "V_pANY_PC"  , "pc"   , 0.001     , 0.1    , "Percent of Population Vaccinated with At Least One Dose"
  ,  "V", "V_pFULL_PC" , "pc"   , 0.001     , 0.1    , "Percent of Population Fully Vaccinated"
  
)


# HEX map shape files 
#' @export
shp_hex_centers <- readRDS("./data/shp_hex_centers.RDS") 
#' @export
shp_hex         <- readRDS("./data/shp_hex.RDS")  


# 57: 
#  50 states
#   1 federal district (DC)
#   5 territories 
#   1 National 
# Valid GEO names (state level)
#' @export
GEOnames <- c(
    "United States"
  , "Alabama"
  , "Alaska"
  , "American Samoa"
  , "Arizona"
  , "Arkansas"
  , "California" 
  , "Colorado"
  , "Connecticut" 
  , "Delaware" 
  , "District of Columbia"
  , "Florida" 
  , "Georgia"
  , "Guam"  
  , "Hawaii"
  , "Idaho" 
  , "Illinois" 
  , "Indiana" 
  , "Iowa" 
  , "Kansas" 
  , "Kentucky" 
  , "Louisiana" 
  , "Maine" 
  , "Mariana Islands" 
  , "Maryland"  
  , "Massachusetts" 
  , "Michigan" 
  , "Minnesota" 
  , "Mississippi" 
  , "Missouri" 
  , "Montana" 
  , "Nebraska"
  , "Nevada" 
  , "New Hampshire" 
  , "New Jersey" 
  , "New Mexico" 
  , "New York" 
  , "North Carolina"
  , "North Dakota" 
  , "Ohio"  
  , "Oklahoma" 
  , "Oregon"
  , "Pennsylvania" 
  , "Puerto Rico" 
  , "Rhode Island"
  , "South Carolina"
  , "South Dakota"  
  , "Tennessee" 
  , "Texas" 
  , "Utah" 
  , "Vermont" 
  , "Virgin Islands" 
  , "Virginia" 
  , "Washington" 
  , "West Virginia" 
  , "Wisconsin" 
  , "Wyoming" 
)


#' Format Dates 
#' @param INDATE 
#' @export
fmt_date <- function(INDATE){
  
  if (class(INDATE) == "Date"){
    THISDATE <- INDATE
  } 
  
  if (class(INDATE) == "character" & nchar(INDATE) == 10){
    THISDATE <- as.Date(INDATE)
  }
  
  OUTDATE <- format(THISDATE, "%b %e, %Y")
  return(OUTDATE)
  
}

