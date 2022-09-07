# R version 4.2.0 (2022-04-22 ucrt) -- "Vigorous Calisthenics"

#######################
# PACKAGES ############

library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(DT)

box::use(
    magrittr[`%>%`]
  , glue[glue]
  , ggplot2[ggsave]
  , box/data
  , box/meta
  , box/plot_map
  , box/table
  , box/plot_chart
)

#######################
# FONTS ###############

##when testing on my machine because I'm on Windooze 
if (Sys.info()[['sysname']] == "Windows"){ extrafont::loadfonts(device = "win", quiet = TRUE) }

##when on shinyapps.io (linux)
if (Sys.info()[['sysname']] == "Linux"){
  dir.create('~/.fonts')
  file.copy("www/fonts/Ubuntu.ttf"     , "~/.fonts")
  file.copy("www/fonts/Ubuntu Mono.ttf", "~/.fonts")
  system('fc-cache -f ~/.fonts')
}


#######################
# DATA ################

DATA_state <- data$load_data() 
DATE_min <- as.Date("2020-01-22")
DATE_max <- max(DATA_state$DATE, na.rm = TRUE)


#######################
# OTHER ###############




