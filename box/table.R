
box::use(
    glue[glue]
  , DT[...]
  , dplyr[mutate, arrange]
  , ./meta
)

#' Create Map DT 
#' @param DT 
#' @param VAR 
#' @export
map_table <- function(DT, VAR){
  
  TYPE  <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$TYPE
  ACC   <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$ACCURACY 
  GROUP <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$GROUP 
  NAME  <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$NAME 
  DIG   <- log(1/ACC, base = 10)
#  digp  <- log(100/ACC, base = 10)
  
  cols_names_old <- c("state_name", "val")
  cols_names_new <- c("Geography Name", NAME)
  
  toShow <- DT[which(state_name %in% meta$GEOnames), c("state_name" , "val")] %>% 
    mutate(state_name = factor(state_name, levels = meta$GEOnames)) %>%
    arrange(desc(val))
  
  data.table::setnames(toShow, cols_names_old, cols_names_new, skip_absent = TRUE)
  
  baseDT <- datatable(
      toShow
    , options = list(
          searchHighlight = TRUE
    ) #end options
  )
  
       if (TYPE == "p100k" & GROUP ==  "C" ){out <- baseDT %>% DT::formatRound( 2, digits = 1  , mark = ",") } 
  else if (TYPE == "p100k" & GROUP !=  "C" ){out <- baseDT %>% DT::formatRound( 2, digits = DIG, mark = ",") } 
  else if (TYPE == "count"                 ){out <- baseDT %>% DT::formatRound( 2, digits = DIG, mark = ",") } 
  else if (TYPE == "pc"                    ){out <- baseDT %>% formatPercentage(2, digits = DIG)             }
  logger::log_info(glue("Create Hex DT for {VAR}"))
  return(out)
}


#' Create Map DT 
#' @param DT 
#' @param VAR 
#' @export
chart_table <- function(DT, VAR){
  
  TYPE  <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$TYPE
  ACC   <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$ACCURACY 
  GROUP <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$GROUP 
  NAME  <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$NAME 
  DIG   <- log(1/ACC, base = 10)
  
  #had issues when using DT, switch to tibble 
  toShow <- dplyr::select(
    dplyr::arrange(
      tidyr::as_tibble(DT), dplyr::desc(val)
    ), 
    DATE, val, state_name
  )
  
  colnames(toShow) <- c("Date", NAME, "Geography Name")
  
  baseDT <- datatable(
    toShow
    , options = list(
      searchHighlight = TRUE
    ) #end options
  )
  
  
  if (TYPE == "p100k" & GROUP ==  "C" ){out <- baseDT %>% DT::formatRound( 2, digits = 1  , mark = ",") } 
  else if (TYPE == "p100k" & GROUP !=  "C" ){out <- baseDT %>% DT::formatRound( 2, digits = DIG, mark = ",") } 
  else if (TYPE == "count"                 ){out <- baseDT %>% DT::formatRound( 2, digits = DIG, mark = ",") } 
  else if (TYPE == "pc"                    ){out <- baseDT %>% formatPercentage(2, digits = DIG)             }
  logger::log_info(glue("Create Chart DT for {VAR}: {DT$state_name[1]}"))
  return(out)
}



