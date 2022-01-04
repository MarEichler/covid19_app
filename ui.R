

ui <- fluidPage(
  titlePanel("MAIN TITLE PANEL"), 
  
  tabsetPanel(type = "tabs", id = "tabsetpanel", selected = 0, #SELECT TAB 
    
    tabPanel(title = "test", value = 0, test_UI("hist1"))
      
  ) #end tabsetPanel 
  
) # end UI ####################################
