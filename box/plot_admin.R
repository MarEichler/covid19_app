
box::use(
  dplyr[case_when]
  , tidyr[tribble]
  , glue[glue]
  , DescTools[RoundTo]
  , scales[percent, comma]
  , stats[quantile]
)

#' Plot Values (round for MA7)
#'
#' @param VAR 
#' @param VAL 
#'
#' @export
plotval <- function(VAL, VAR){
  if (VAR %in% c("C_MA7", "C_MA7_P100K")){
    out <- ifelse(VAL >  1.5, round(VAL, digits = 0), round(VAL, digits = 1))
  } else {
    out <- round(VAL, digits = 0)
  }
  return(out)
}


#' Color DF with breaks, labels, colors 
#'
#' @param VAR 
#' @param VALS 
#' @param TYPE 
#'
#' @return
#' @export
#'
#' @examples
coldf <- function(VAR, VALS, TYPE = "comma"){

  if (VAR %in% c("C_MA7_P100K", "C_NEW_P100K")){
    coldf <- tribble(
      ~breaks, ~labels, ~colors, ~font
      , c(-Inf, 1, 10, 25, 50, Inf) 
      , c("<1 Low" , "[1,10) Medium", "[10,25) High", "[25,50) Very High", "50+ Extremely High")
      , c("#51A09E", "#EFC637"      , "#E38D2C"     , "#A23520"          , "#87216B")
      , c("black"  ,  "black"       , "black"       , "white"            , "white")
    )
  }
  
  quant_perc <- c(0, 0.2, 0.4, 0.6, 0.8, 1) 
  
  if (VAR %in% c("C_NEW", "C_CUM")){
    
    breaks <- RoundTo(quantile(x = VALS, prob = quant_perc), 100)
    
    coldf <- tribble(
      ~breaks, ~labels, ~colors, ~font
      , breaks 
      , set_quant_labels(breaks, TYPE)
      , c("#51A09E", "#EFC637"      , "#E38D2C"     , "#A23520"          , "#87216B")
      , c("black"  ,  "black"       , "black"       , "white"            , "white")
    )
  }
  
  return(coldf)
  
}


#' Title
#'
#' @param VAL 
#' @param TYPE 
#'
#' @return
#' @export
#'
#' @examples
displayval <- function(VAL, TYPE){
  if (TYPE == "comma"){
    comma(VAL)
  }
}


#' Titl
#' @param BREAKS 
#' @param TYPE 
set_quant_labels <- function(BREAKS, TYPE){
  if (TYPE == "comma"){
    labels <- c(
        glue("<{comma(BREAKS[2])}")
      , glue( "{comma(BREAKS[2])} - {comma(BREAKS[3]-1)}")
      , glue( "{comma(BREAKS[3])} - {comma(BREAKS[4]-1)}")
      , glue( "{comma(BREAKS[4])} - {comma(BREAKS[5]-1)}")
      , glue( "{comma(BREAKS[5])}+")
    )
  }
}


#' Set Font Color 
#'
#' @param COLGROUP 
#' @param COLDF 
#' @export
fontcol <- function(COLGROUP, COLDF){
  case_when(
      COLGROUP == COLDF$labels[[1]][1] ~ COLDF$font[[1]][1]
    , COLGROUP == COLDF$labels[[1]][2] ~ COLDF$font[[1]][2]
    , COLGROUP == COLDF$labels[[1]][3] ~ COLDF$font[[1]][3]
    , COLGROUP == COLDF$labels[[1]][4] ~ COLDF$font[[1]][4]
    , COLGROUP == COLDF$labels[[1]][5] ~ COLDF$font[[1]][5]
  )
}


risk_group_colors <- c(
  "#51A09E" #<1 
  , "#EFC637" # [1,10)
  , "#E38D2C" # [10,25)
  , "#A23520" # [25, 50)
  , "#87216B" #[50, inf)
)


#' Title for Plot 
#'
#' @param VAR 
#' @param VAL 
#'
#' @export
plottitle <- function(VAR, DATE){
  fmt_date <- format(DATE, "%b %d, %Y")
  if (VAR == "C_MA7_P100K"){title <- glue("New Cases 7-Day Moving Average per 100,000 as of {fmt_date}")}
  if (VAR == "C_NEW_P100K"){title <- glue("New Cases per 100,000 as of {fmt_date}")}
  if (VAR == "C_NEW"      ){title <- glue("New Cases as of {fmt_date}")}
  if (VAR == "C_CUM"      ){title <- glue("Total Cases as of {fmt_date}")}
  return(title)
}
