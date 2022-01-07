
#' @export
VAROPTS <- tibble::tribble(
  ~GROUP,     ~VAR      , ~TYPE  ,  ~RNDVAL  , ~ACCURACY, ~NAME
  ,  "C", "C_CUM"      , "count", 100       , 1      , "Total Cases"
  ,  "C", "C_NEW"      , "count", 10        , 1      , "Daily Cases"
  ,  "C", "C_MA7"      , "count", 10        , 1      , "Daily Cases 7-Day Moving Average"
  ,  "C", "C_CUM_PC"   , "pc"   , 0.01      , 0.1    , "Total Cases as Percent of Population"
# ,  "C", "C_NEW_PC"   , "pc"   , 0.0000001 , 0.00001, "Daily Cases Per Capita"
# ,  "C", "C_MA7_PC"   , "pc"   , 0.0000001 , 0.00001, "Daily Cases 7-Day Moving Average Per Capita"
# ,  "C", "C_CUM_P100K", "p100k", 10        , 1      , "Total Cases per 100,000"
  ,  "C", "C_NEW_P100K", "p100k", 1         , 1      , "Daily Cases per 100,000"
  ,  "C", "C_MA7_P100K", "p100k", 1         , 1      , "Daily Cases 7-Day Moving Average per 100,000"
  ,  "D", "D_CUM"      , "count", 100       , 1      , "Total Deaths"
  ,  "D", "D_NEW"      , "count", 10        , 1      , "Daily Deaths"
  ,  "D", "D_MA7"      , "count", 10        , 1      , "Daily Deaths 7-Day Moving Average"
  ,  "D", "D_CUM_PC"   , "pc"   , 0.0001    , 0.01   , "Total Deaths as Percent of Population"
# ,  "D", "D_NEW_PC"   , "pc"   , 0.00000001, 0.00001, "Daily Deaths Per Capita"
# ,  "D", "D_MA7_PC"   , "pc"   , 0.00000001, 0.00001, "Daily Deaths 7-Day Moving Average Per Capita"
# ,  "D", "D_CUM_P100K", "p100k", 10        , 1      , "Total Deaths per 100,000"
  ,  "D", "D_NEW_P100K", "p100k", 0.01      , 0.01   , "Daily Deaths per 100,000"
  ,  "D", "D_MA7_P100K", "p100k", 0.01      , 0.01   , "Daily Deaths 7-Day Moving Average per 100,000"
)


# HEX map shape files 
#' @export
shp_hex_centers <- readRDS("./data/shp_hex_centers.RDS") 
#' @export
shp_hex         <- readRDS("./data/shp_hex.RDS")  
