# R version 4.1.1 (2021-08-10) -- "Kick Things"

library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(ggplot2)

box::use(
    magrittr[`%>%`]
  , glue[glue]
  , box/meta 
  , box/plot_map
  , box/data
)


DT <- readRDS("state.RDS")
default_width  <- 8
default_height <- 5.25
extrafont::loadfonts(device = "win", quiet = TRUE) #loads fonts 


