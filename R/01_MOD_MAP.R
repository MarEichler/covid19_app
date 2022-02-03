

map_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      mod_filters_UI(ns("var")), 
      mainPanel(
        tabsetPanel(type = "tabs", id = "tabsetpanel_map", selected = 11, #SELECT TAB 
          tabPanel(
            title = "Hex Map",  
            value = 11, 
            br(), 
            imageOutput(ns("map"), width = "98%", height = "98%") %>% withSpinner()
            ), #end tabPanle<Hex Map
          tabPanel(
            title = "Table", 
            value = 12, 
            br(), 
            DTOutput(ns("table"), width = "98%") %>% withSpinner()
            ) #end tabPanel<Table
        ) #end tabsetPanel<tabsetpanle_map
      ) #end mainPanel 
    ) #end sidebarLayout
  ) #end tagList 
} #end map_UI


map_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # variables from mod_filters_Server 
    var <- mod_filters_Server("var", nDATES = 1)
    
    # subset data needed 
    data <- reactive({ 
      req( var$date() )
      plot_map$PlotDT_map( DT = DATA_state, VAR = var$metric(), INDATE = var$date() ) 
    }) #end reactive<data
    
    # create plot 
    p <- reactive({ plot_map$create_map( DT = data()$DT, VAR = data()$VAR )  }) #end reactive<p
    
    # create image 
    output$map <- renderImage({
      plotit <- p()
      out_width   <- session$clientData[[glue("output_{id}-map_width" )]]
      outfile <- tempfile(fileext = ".png")
      ggsave(file = outfile, plot = plotit, width = 8, height = 5.25)
      list(
        src         = normalizePath(outfile)
        , width       = out_width
        , height      = out_width/(8/5.25)
        , contentType = "image/png"
        , alt         = "alt text palce holder"
      )
    }, deleteFile = TRUE) #end renderImage<output$map
    
    
    output$table <- renderDT({
      
     table$map_table(DT = data()$DT, VAR = data()$VAR )
      
    }) #end renderDT<output$table
    
    
    
  }) #end moduleServer 
} #end map_Server

