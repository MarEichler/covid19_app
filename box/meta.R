
#' @export
VAROPTS <- tibble::tribble(
       ~VAR      , ~TYPE  ,  ~RNDVAL  , ~ACCURACY, ~NAME
  , "C_CUM"      , "count", 100       , 1      , "Total Cases"
  , "C_NEW"      , "count", 10        , 1      , "Daily Cases"
  , "C_MA7"      , "count", 10        , 1      , "Daily Cases 7-Day Moving Average"
  , "C_CUM_PC"   , "pc"   , 0.01      , 0.1    , "Total Cases Per Capita"
# , "C_NEW_PC"   , "pc"   , 0.0000001 , 0.00001, "Daily Cases Per Capita"
# , "C_MA7_PC"   , "pc"   , 0.0000001 , 0.00001, "Daily Cases 7-Day Moving Average Per Capita"
  , "C_CUM_P100K", "p100k", 10        , 1      , "Total Cases per 100,000"
  , "C_NEW_P100K", "p100k", 1         , 1      , "Daily Cases per 100,000"
  , "C_MA7_P100K", "p100k", 1         , 1      , "Daily Cases 7-Day Moving Average per 100,000"
  , "D_CUM"      , "count", 100       , 1      , "Total Deaths"
  , "D_NEW"      , "count", 10        , 1      , "Daily Deaths"
  , "D_MA7"      , "count", 10        , 1      , "Daily Deaths 7-Day Moving Average"
  , "D_CUM_PC"   , "pc"   , 0.0001    , 0.01   , "Total Deaths Per Capita"
# , "D_NEW_PC"   , "pc"   , 0.00000001, 0.00001, "Daily Deaths Per Capita"
# , "D_MA7_PC"   , "pc"   , 0.00000001, 0.00001, "Daily Deaths 7-Day Moving Average Per Capita"
  , "D_CUM_P100K", "p100k", 10        , 1      , "Total Deaths per 100,000"
  , "D_NEW_P100K", "p100k", 0.1       , 0.1    , "Daily Deaths per 100,000"
  , "D_MA7_P100K", "p100k", 0.1       , 0.1    , "Daily Deaths 7-Day Moving Average per 100,000"
)


# HEX map shape files 
#' @export
shp_hex_centers <- readRDS("./data/shp_hex_centers.RDS") 
#' @export
shp_hex         <- readRDS("./data/shp_hex.RDS")  
