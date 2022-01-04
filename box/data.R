
box::use(
    ./link
  , lubridate[mdy]
  , typed[...]
  , data.table[...]
  , logger[...]
)


#####################################
#' Pull Raw Data from JH
#' @param LINK Character(1) - character vector of length 1 
#' @param TYPE Character(1) - character vector of length 1 
#' @return Long DF where each date and cumulative county are on rows 
#' @examples pull_JH(link$jh_cases, "C")
#' @examples pull_JH(link$jh_dealth, "D")
pull_JH <- ? function(
    LINK = ? Character(1)
  , TYPE = ? Character(1, ... = "Need to be C = Cases, D = Dealths" ~ . %in% c("C", "D"))
  ){
  # LOAD IN DATA 
  DT00 <- fread(LINK) 
  
  # KEEP SPECIFIC COLUMNS AND RENAME 
  cols_to_keep <- c(which(colnames(DT00) %in% c("FIPS", "Admin2", "Province_State")), grep("/", colnames(DT00)))
  DT <- DT00[, ..cols_to_keep] 
  setnames(DT, c("FIPS", "Admin2", "Province_State"), c("county_fips", "county_name", "state_name"), skip_absent = TRUE)
  
  #convert names to match URBNMAPR data 
  setDT(DT)[, `:=` (state_name = ifelse(state_name == "Northern Mariana Islands", "Mariana Islands", state_name))]
  
  # create county_fips (this is because population is available for PR as a whole, not by county fips)
  setDT(DT)[, `:=` (county_fips = ifelse(state_name  == "Puerto Rico",            72, county_fips))]
  
  #clean up county/state names 
  setDT(DT)[, `:=` (county_name = ifelse(state_name  == "Puerto Rico", "Puerto Rico", county_name))]
  setDT(DT)[, `:=` (county_name = ifelse(county_name == ""            , state_name  , county_name))]
  
  #sum accounts over new county_fips (which will group PR values together) 
  date_cols <- grep("/", colnames(DT))
  DT <- DT[, lapply(.SD, sum), by = c("county_fips", "county_name", "state_name"), .SDcols = date_cols]
  
  #melt (pivot longer) so dates are in rows instead of column names 
  DT <- melt(DT, id.vars = c("county_fips", "county_name", "state_name"), variable.name = "DATE", value.name = paste0(TYPE, "_CUM"))
  
  #convert dates from character to numeric dates 
  setDT(DT)[, `:=` (DATE = mdy(DATE))]
  
  return(DT)
}

####################################3
#' Return Data after pulled 
raw_data <- function(){
  log_info("start JH pull")
  jh_C <- pull_JH(link$jh_cases, "C")
  log_info("pull Cases from JH github")
  jh_D <- pull_JH(link$jh_deaths, "D")
  log_info("pull Deaths from JH github")
  DT <- merge(jh_C, jh_D)[order(county_fips),]
  DT[order(county_fips),]
  log_info("end JH pull")
  return(DT)
}

##############################
# functions for state/county dt creation

#' Calculate New Cases by Taking Today's Cumulative Cases - Yesterday's Cumulative Cases
#' @param CUM 
calc_new <- function(CUM, DATE){
  startdate <- as.Date("2020-01-22")
  NEW <- CUM - shift(CUM)
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
#' @export
#' @examples
create_dt  <- Data.frame() ? function(
    RAWDT = ? Data.frame()
  , POPDT = ? Data.frame()
  , LEV   = ? Character(1, ... = "Need to be STATE or CNTY" ~ . %in% c("STATE", "CNTY"))
  ){
  
  DT <- RAWDT
  
  #order 
  log_info("{LEV} order columns")
  DT <- DT[DT[,do.call(order, .SD), .SDcols = c("state_name", "county_fips", "DATE")]]
  
  if (LEV == "STATE"){
    log_info("{LEV} sum up by STATE level")
    DT <- DT[, lapply(.SD, sum), by = c("state_name", "DATE"), .SDcols = c("C_CUM", "D_CUM")]
    
    usa <- DT[, lapply(.SD, sum), by = c("DATE"), .SDcols = c("C_CUM", "D_CUM")]
    setDT(usa)[, `:=` (state_name = "United States")]
    
    DT <- rbind(usa, DT)
  }
  
  #combine with population data 
  log_info("{LEV} merge raw dt with population data ")
  DT <- merge(DT, POPDT, all.x = TRUE)
  
  #calculate new cases 
  log_info("{LEV} creating NEW columsn")
  setDT(DT)[, `:=`
            (   C_NEW = calc_new(C_CUM, DATE)
              , D_NEW = calc_new(D_CUM, DATE)
            ), by = county_fips]
  
  #calculate 7day moving average 
  log_info("{LEV} creating MA7 columsn")
  setDT(DT)[, `:=`
            (   C_MA7 = c(rep(NA, 6), zoo::rollmean(C_NEW, k = 7, na.rm = TRUE))
              , D_MA7 = c(rep(NA, 6), zoo::rollmean(D_NEW, k = 7, na.rm = TRUE))
            ), by = county_fips]
  
  
  #start calculating proportional columns 
  log_info("{LEV} creating PROP columns")
  setDT(DT)[,`:=`
            (   C_CUM_P100K = calc_p100K(C_CUM, POP)
              , C_CUM_PC    = calc_pc(   C_CUM, POP)
              , C_NEW_P100K = calc_p100K(C_NEW, POP)
             #, C_NEW_PC    = calc_pc(   C_NEW, POP)
              , C_MA7_P100K = calc_p100K(C_MA7, POP)
             #, C_MA7_PC    = calc_pc(   C_MA7, POP)
              , D_CUM_P100K = calc_p100K(D_CUM, POP)
              , D_CUM_PC    = calc_pc(   D_CUM, POP)
              , D_NEW_P100K = calc_p100K(D_NEW, POP)
             #, D_NEW_PC    = calc_pc(   D_NEW, POP)
              , D_MA7_P100K = calc_p100K(D_MA7, POP)
             #, D_MA7_PC    = calc_pc(   D_MA7, POP)
            )]
  
  log_success("{LEV} complete creating DT")
  return(DT)
} #end create_dt 


#####################################

#' Pull/Create Data Tables 
#' @export
get_data <- function(){
  rawDT     <- raw_data()
  pop_cnty  <- readRDS("data/pop_cnty.RDS") 
  pop_state <- readRDS("data/pop_state.RDS")
  
  out <- list()
  out$cntyDT  <- create_dt(rawDT, pop_cnty,  "CNTY")
  out$stateDT <- create_dt(rawDT, pop_state, "STATE")
  
  return(out)
}






