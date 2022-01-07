

map_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      mod_filters_UI(ns("var")), 
      mainPanel(
        #width = 8, 
        #plotOutput(ns("map"), width = "100%") %>% withSpinner() 
        imageOutput(ns("map"), width = "100%") %>% withSpinner()
      ) #end mainPanel 
    ) #end sidebarLayout
  ) #end tagList 
} #end map_UI


map_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    var <- mod_filters_Server("var")
    
    # output$map <- renderPlot({
    #   ggplot2::ggplot() + ggplot2::labs( title = paste0("this is the text ", var$metric()), subtitle = "this is the text") 
    # 
    # })
    
    p <- reactive({
      #ggplot2::ggplot() + ggplot2::labs( title = paste0("this is the text ", var$metric()), subtitle = "this is the text")
      plot_map$create_map( DATA_state, var$metric() ) 

    })

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
    
    #output$map <- renderPlot({  plot_map$create_map( DATA_state, var$metric() )  })
    

    
    # output$map <- renderImage({
    #   req(var$metric())
    #   p       <- plot_map$create_map( DATA_state, var$metric() )
    #   out_width   <- session$clientData[[glue("output_{id}-map_width" )]]
    #   outfile <- tempfile(fileext = ".jpg")
    #   ggsave(file = outfile, plot = p, width = 8, height = 5.25)
    #   list(
    #       src         = normalizePath(outfile)
    #     , width       = out_width
    #     , height      = out_width * (5.25/8) 
    #     , contentType = "image/jpg"
    #     , alt         = "alt text palce holder"
    #   )
    # }, deleteFile = TRUE) #end renderImage<output$map
    
    
    
  }) #end moduleServer 
} #end map_Server

