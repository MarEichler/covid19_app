

ui <- fluidPage(
  titlePanel("COVID-19: USA"), 
  tags$html(lang="en"), 
  tabsetPanel(type = "tabs", id = "tabsetpanel", selected = 2, #SELECT TAB 
    #tabPanel(title = "test",       value = 0, br(), test_UI("hist1")),  
    tabPanel(title = "Snap Shot", value = 1, br(), map_UI("map")), 
    tabPanel(title = "Trends"   , value = 2, br(), chart_UI("chart"))
  ) #end tabsetPanel 
  
) # end UI ####################################
