


mod_filters_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(width = 4, 
      pickerInput(
        inputId    = ns("metric"), 
        label      = "Metric", 
        choices    = list( #actual choices that will be used in server 
          Cases    = meta$VAROPTS[which(meta$VAROPTS$GROUP == "C"),]$VAR, 
          Deaths   = meta$VAROPTS[which(meta$VAROPTS$GROUP == "D"),]$VAR,
          Vaccines = meta$VAROPTS[which(meta$VAROPTS$GROUP == "V"),]$VAR
        ), #end list<choices 
        choicesOpt = list(content = meta$VAROPTS$NAME)   #names to show in drop down 
      ), #end pickerInput 
      uiOutput(ns("date_ui")), 
      uiOutput(ns("daterng_ui")), 
      uiOutput(ns("geo_ui"))
    ) #end sidebarPanel 
  ) #end tagList
} #end mod_filters_UI


mod_filters_Server <- function(id, nDATES = 1, GEO = FALSE) {
  moduleServer(id, function(input, output, session) {
    
    ###############
    ## ONE DATE
    output$date_ui <- renderUI({
      #req(ONEDATE)
      req(nDATES == 1)
      ns <- session$ns
      dateInput(
        inputId = ns("date"),
        label   = glue("Select Date (2020-01-22 to {DATE_max})"), 
        min     = DATE_min, 
        max     = DATE_max, 
        value   = DATE_max
      )
    })
    
    observe({
      met   <- input$metric
      GROUP <- substr(met, 1, 1)
      req ( GROUP == "V" )
      updateDateInput(session,
        inputId = "date",
        label   = glue("Select Date (2021-01-13 to {DATE_max})"),
        min     = "2021-01-13"
      ) #end updateDateInput
    }) #end observe 
    
    ###############
    ## TWO DATE
    output$daterng_ui <- renderUI({
      #req(ONEDATE)
      req(nDATES == 2)
      ns <- session$ns
      dateRangeInput(
        inputId = ns("daterng"),
        label   = glue("Select Date (2020-01-22 to {DATE_max})"), 
        min     = DATE_min, 
        max     = DATE_max, 
        start   = DATE_min, 
        end     = DATE_max
      )
    })
    
    observe({
      met   <- input$metric
      GROUP <- substr(met, 1, 1)
      req ( GROUP == "V" )
      updateDateRangeInput(session,
                      inputId = "daterng",
                      label   = glue("Select Date (2021-01-13 to {DATE_max})"),
                      min     = "2021-01-13", 
                      start   = "2021-01-13"
      ) #end updateDateInput
    }) #end observe 
    
    ###############
    ## GEO INPUT
    output$geo_ui <- renderUI({
      req(GEO) 
      ns <- session$ns
      pickerInput(
        inputId = ns("geo"), 
        label   = "Geographic Area", 
        choices = meta$GEOnames) #end pickerInput
    }) #end renderUI<pickerInput<geo
    
    
     return(list(
       metric  = reactive(input$metric), 
       date    = reactive(input$date), 
       daterng = reactive(input$daterng), 
       geo     = reactive(input$geo) 
     )) #end return(list(
  }) #end moduleServer
} #end mod_filters_server