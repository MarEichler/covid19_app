

chart_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      mod_filters_UI(ns("var")), 
      mainPanel(
        tabsetPanel(type = "tabs", id = "tabsetpanel_chart", selected = 21, #SELECT TAB 
          tabPanel(
            title = "Chart",  
            value = 21, 
            br(), 
            imageOutput(ns("chart"), width = "98%", height = "98%") %>% withSpinner()
            ), #end tabPanle<Chart
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


chart_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # variables from mod_filters_Server 
    var <- mod_filters_Server("var", nDATES = 2, GEO = TRUE )
    
    # subset data needed
    data <- reactive({
      req( var$daterng()[1] )
      req( var$daterng()[2] )
      plot_chart$PlotDT_chart(DT = DATA_state, VAR = var$metric(), GEO = var$geo(), DATE_RNG = var$daterng() )
    })
    
    p <- reactive({ 
      plot_chart$create_chart( DT = data()$DT, VAR = data()$VAR, GEO = data()$GEO ) 
    })
    
    # create image 
    output$chart <- renderImage({
      plotit <- p()
      out_width   <- session$clientData[[glue("output_{id}-chart_width" )]]
      outfile <- tempfile(fileext = ".png")
      ggsave(file = outfile, plot = plotit, width = 8, height = 4.25)
      list(
        src         = normalizePath(outfile)
        , width       = out_width
        , height      = out_width/(8/4.25)
        , contentType = "image/png"
        , alt         = "alt text palce holder"
      )
    }, deleteFile = TRUE) #end renderImage<output$map
    
    
    output$table <- renderDT({

     DT::datatable( data()$DT )

    }) #end renderDT<output$table
    
    
    
  }) #end moduleServer 
} #end map_Server

