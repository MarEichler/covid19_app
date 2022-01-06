# R version 4.1.1 (2021-08-10) -- "Kick Things"

library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(ggplot2)


library(data.table)

library(readr)

box::use(
    magrittr[`%>%`]
  , glue[glue]
  , box/data
  , box/meta
  , box/plot_map
)

link_jh_cases  <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
# data_dot <- vroom::vroom(link_jh_cases)
# data.table::setDT(data_dot)

DT_lst <- data$get_data(POPSTATE = readRDS("data/pop_state.RDS"), POPCNTY = readRDS("data/pop_cnty.RDS") )

DATA_state <- DT_lst$stateDT


#DATA_state <- readRDS("data/state.RDS")
default_width  <- 8
default_height <- 5.25
#extrafont::loadfonts(device = "win", quiet = TRUE) #loads fonts 


