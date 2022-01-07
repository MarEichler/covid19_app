

server <- function(input, output, session) {
    test_Server("hist1")
    map_Server("map")
    
    # var <- mod_filters_Server("var")
    # 
    # p <- reactive({
    #   ggplot2::ggplot() + ggplot2::labs( title = paste0("this is the text "), subtitle = "this is the text")
    #   
    # })
    # 
    # output$map <- renderImage({
    #   plotit <- p()
    #   out_width   <- session$clientData$output_map_width
    #   out_height  <- session$clientData$output_map_height 
    #   outfile <- tempfile(fileext = ".png")
    #   ggsave(file = outfile, plot = plotit, width = 8, height = 5.25)
    #   list(
    #     src         = normalizePath(outfile)
    #     , width       = out_width
    #     , height      = out_width/(8/5)
    #     , contentType = "image/png"
    #     , alt         = "alt text palce holder"
    #   )
    # }, deleteFile = TRUE) #end renderImage<output$map
    
    
} # end SERVER ##############################
