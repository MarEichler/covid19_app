# R version 4.1.1 (2021-08-10) -- "Kick Things"

#######################
# PACKAGES ############

library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(ggplot2)

box::use(
    magrittr[`%>%`]
  , glue[glue]
  , box/data
  , box/meta
  , box/plot_map
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

# font_name <- "Ubuntu"
# font_mono <- "Ubuntu Mono"


#######################
# DATA ################

DT_lst <- data$get_data(
    POPSTATE = readRDS("data/pop_state.RDS")
 # , POPCNTY = readRDS("data/pop_cnty.RDS")
  )
DATA_state <- DT_lst$stateDT

# DATA_state <- readRDS("state.RDS")


#######################
# OTHER ###############




