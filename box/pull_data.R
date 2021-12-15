
box::use(
    ./link
  , lubridate[mdy]
  , typed[...]
  , data.table[...]
  , logger[...]
)



#' Pull Raw Data from JH
#'
#' @param LINK Character(1) - character vector of length 1 
#' @param TYPE Character(1) - character vector of length 1 
#'
#' @return Long DF where each date and cumulative county are on rows 
#' @examples
#' pull_JH(link$jh_cases, "C")
#' pull_JH(link$jh_dealth, "D")
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
  
  #sum acounts over new county_fips (which will group PR values together) 
  date_cols <- grep("/", colnames(DT))
  DT <- DT[, lapply(.SD, sum), by = c("county_fips", "county_name", "state_name"), .SDcols = date_cols]
  
  #melt (pivot longer) so dates are in rows instead of column names 
  DT <- melt(DT, id.vars = c("county_fips", "county_name", "state_name"), variable.name = "DATE", value.name = paste0(TYPE, "_CUM"))
  
  #convert dates from character to numeric dates 
  setDT(DT)[, `:=` (DATE = mdy(DATE))]
  
  return(DT)
}


#' Return Data after pulled 
#'
raw_data <- function(){
  jh_C <- pull_JH(link$jh_cases, "C")
  log_success("pull Cases from JH github")
  jh_D <- pull_JH(link$jh_deaths, "D")
  log_success("pull Ceaths from JH github")
  DT <- merge(jh_C, jh_D)[order(county_fips),]
  DT[order(county_fips),]
  return(DT)
}

rawDT <- raw_data()


