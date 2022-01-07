

ui <- fluidPage(
  titlePanel("MAIN TITLE PANEL"), 
  
  tabsetPanel(type = "tabs", id = "tabsetpanel", selected = 1, #SELECT TAB 
    
    tabPanel(title = "test",       value = 0, br(), test_UI("hist1")),  
    tabPanel(title = "Snap Shot" , value = 1, br(), map_UI("map")), 
    # tabPanel(title = "Snap Shot 2", value = 1, br(), 
    #          sidebarLayout(
    #            mod_filters_UI("var"), 
    #            mainPanel(
    #              #width = 8, 
    #              #plotOutput("map"), width = "100%") %>% withSpinner() 
    #              imageOutput("map", width = "100%") %>% withSpinner()
    #            ) #end mainPanel 
    #          ) #end sidebarLayout
    #   )#end tabpaenl 
      
  ) #end tabsetPanel 
  
) # end UI ####################################
