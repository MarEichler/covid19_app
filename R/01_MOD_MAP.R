

map_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      mod_filters_UI(ns("var")), 
      mainPanel(
        imageOutput(ns("map"), width = "100%") %>% withSpinner()
      ) #end mainPanel 
    ) #end sidebarLayout
  ) #end tagList 
} #end map_UI


map_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    var <- mod_filters_Server("var", ONEDATE = TRUE)
    
    data <- reactive({ 
      req( var$date() )
      plot_map$PlotDT_map( DT = DATA_state, VAR = var$metric(), INDATE = var$date() ) 
    })
    
    p <- reactive({ plot_map$create_map( DT = data()$DT, VAR = data()$VAR )  })

    output$map <- renderImage({
      
      plotit <- p()
      out_width   <- session$clientData[[glue("output_{id}-map_width" )]]
      outfile <- tempfile(fileext = ".png")
      ggsave(file = outfile, plot = plotit, width = 8, height = 5.25)
      list(
        src         = normalizePath(outfile)
        , width       = out_width
        , height      = out_width/(8/5)
        , contentType = "image/png"
        , alt         = "alt text palce holder"
      )
      
    }, deleteFile = TRUE) #end renderImage<output$map
    
    
    
  }) #end moduleServer 
} #end map_Server

