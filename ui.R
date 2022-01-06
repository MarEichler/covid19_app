

ui <- fluidPage(
  titlePanel("MAIN TITLE PANEL"), 
  
  tabsetPanel(type = "tabs", id = "tabsetpanel", selected = 0, #SELECT TAB 
    
    tabPanel(title = "test", value = 0, br(), test_UI("hist1")),  
    tabPanel(title = "map" , value = 1, br(), map_UI("hist2"))
      
  ) #end tabsetPanel 
  
) # end UI ####################################
