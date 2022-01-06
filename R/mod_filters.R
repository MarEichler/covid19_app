


mod_filters_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(width = 4, 
      pickerInput(
        inputId    = ns("metric"), 
        label      = "Metric", 
        choices    = list( #actual choices that will be used in server 
          Cases  = meta$VAROPTS[which(meta$VAROPTS$GROUP == "C"),]$VAR, 
          Deaths = meta$VAROPTS[which(meta$VAROPTS$GROUP == "D"),]$VAR
        ), #end list<choices 
        choicesOpt = list(content = meta$VAROPTS$NAME)   #names to show in drop down 
      ) #end pickerInput 
    ) #end sidebarPanel 
  ) #end tagList
} #end mod_filters_UI


mod_filters_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
     return(list(
       metric = reactive(input$metric)
     )) #end return(list(
  }) #end moduleServer
} #end mod_filters_server