

map_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      mod_filters_UI(ns("var")), 
      mainPanel(
        width = 8, 
        plotOutput(ns("map"), width = "100%") %>% withSpinner() 
        #imageOutput(ns("map"), width = "100%", height = "100%") %>% withSpinner()
      ) #end mainPancel 
    ) #end sidebarLayout
  ) #end tagList 
} #end map_UI


map_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    var <- mod_filters_Server("var")
    
    #output$map <- renderPlot({ ggplot2::ggplot() + ggplot2::ggtitle( var$metric() ) })
    
    output$map <- renderPlot({
      plot_map$create_map( DATA_state, var$metric() )
    })
    
    
    
    # output$map <- renderImage({
    #   req(var$metric())
    #   p       <- plot_map$create_map( DT, var$metric() )
    #   out_width   <- session$clientData[[glue("output_{id}-map_width" )]]
    #   outfile <- tempfile(fileext = ".jpg")
    #   ggsave(file = outfile, plot = p, width = default_width, height = default_height)
    #   list(
    #       src         = normalizePath(outfile)
    #     , width       = out_width
    #     , contentType = "image/jpg"
    #     , alt         = "alt text palce holder"
    #   )
    # }, deleteFile = TRUE) #end renderImage<output$map
    
    
    
  }) #end moduleServer 
} #end map_Server

