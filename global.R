# R version 4.1.1 (2021-08-10) -- "Kick Things"

#######################
# PACKAGES ############

library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(ggplot2)
library(DT)

box::use(
    magrittr[`%>%`]
  , glue[glue]
  , box/data
  , box/meta
  , box/plot_map
  , box/table
)

#######################
# FONTS ###############

##when testing on my machine because I'm on Windooze 
if (Sys.info()[['sysname']] == "Windows"){ extrafont::loadfonts(device = "win", quiet = TRUE) }

##when on shinyapps.io (linux)
if (Sys.info()[['sysname']] == "Linux"){
  dir.create('~/.fonts')
  file.copy("fonts/Ubuntu.ttf"     , "~/.fonts")
  file.copy("fonts/Ubuntu Mono.ttf", "~/.fonts")
  system('fc-cache -f ~/.fonts')
}


#######################
# DATA ################

now_datetime <- lubridate::with_tz(Sys.time(), tzone = "America/Los_Angeles")
run_date     <- readRDS("data/run_date.RDS")

if (now_datetime < run_date ){
  DATA_state <- readRDS("data/DATA_state.RDS")
  logger::log_info("used data in folder")
} else {
  DATA_state <- data$get_data(POPSTATE = readRDS("data/pop_state.RDS"))
  saveRDS(DATA_state, "data/DATA_state.RDS")
  run_date <- glue("{as.Date(format(now_datetime, '%Y-%m-%d'))+1} 00:01:00 {format(now_datetime, '%Z')}")
  saveRDS(run_date, "data/run_date.RDS")
  logger::log_info("pull data from JH github and update data in the folder")
}

DATE_min <- as.Date("2020-01-22")
DATE_max <- max(DATA_state$DATE, na.rm = TRUE)


#######################
# OTHER ###############




