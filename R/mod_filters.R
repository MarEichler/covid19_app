


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
      uiOutput(ns("date_ui"))
    ) #end sidebarPanel 
  ) #end tagList
} #end mod_filters_UI


mod_filters_Server <- function(id, ONEDATE = FALSE, TWODATES = FALSE) {
  moduleServer(id, function(input, output, session) {
    
    ## ONE DATE
    output$date_ui <- renderUI({
      req(ONEDATE)
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
    
    
     return(list(
       metric = reactive(input$metric), 
       date   = reactive(input$date)
     )) #end return(list(
  }) #end moduleServer
} #end mod_filters_server