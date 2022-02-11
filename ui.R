

ui <- fluidPage(
  theme = "style.css", 
  tags$html(lang="en"),
  tags$title("COVID-19: USA"), 
  fluidRow(
    column(
      width = 12, 
      h1("COVID-19 in the United States"), 
      tabsetPanel(type = "tabs", id = "tabsetpanel", selected = 2, #SELECT TAB 
        #tabPanel(title = "test",       value = 0, br(), test_UI("hist1")), 
        tabPanel(title = "Snap Shot"  , value =  1, br(), map_UI("map")), 
        tabPanel(title = "Trends"     , value =  2, br(), chart_UI("chart")),
        tabPanel(title = "Data Source", value = 99, br(), fluidRow(column(width = 12, htmlTemplate("www/datasource.html"))))
      ),  #end tabsetPanel 
      #-- universal footer -------
      br(), br(), 
    ) #end column 
  ) #end fluidRow
) # end UI ####################################
