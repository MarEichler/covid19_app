
box::use(
    dplyr[...]
  , ggplot2[...]
  , tibble[tribble]
  , glue[glue]
  , scales[percent, comma]
  , ./meta
)


#' Determine which scaling to use 
#' @param TYPE 
#' @param SUFFIX 
which_Y_scale <- function(TYPE, SUFFIX){
  if (TYPE == "pc")                 {out<-lst(ggplot2::scale_y_continuous(name=NULL, label=scales::percent_format()             , expand=c(0, 0)))}
  if (TYPE %in% c("count", "p100k")){out<-lst(ggplot2::scale_y_continuous(name=NULL, label=scales::comma_format(suffix = SUFFIX), expand=c(0, 0)))}
  return(out)
}


#' Subset main DT for plot data 
#' @param DT 
#' @param VAR 
#' @param INDATE 
#' @export
PlotDT_chart <- function(DT, VAR, GEO, DATE_RNG){
  
  DT    <- select(
    filter(DT
           , state_name == GEO
           , DATE >= DATE_RNG[1]
           , DATE <= DATE_RNG[2])
    , DATE, val = all_of(VAR), state_name)
  return(list(
      DT   = DT
    , VAR  = VAR
    , NAME = meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$NAME 
    , GEO  = GEO
    , DATE_RNG = DATE_RNG
  ))
  
}



#' Create Line chart 
#' @param DT 
#' @param VAR 
#' @param GEO
#' @export
create_chart <- function(DT, VAR, GEO){
  
  toPlot    <- DT
  plottitle <- glue("{meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$NAME}: {GEO}")

  TYPE   <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$TYPE
  ACC    <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$ACCURACY

  y_breaks <- scales::pretty_breaks(n= 6)(toPlot$val)
  E <- round(mean(log(y_breaks[which(y_breaks != 0)], base = 10))-1, digits = 0)
  DIVIDE <- case_when(E <  3         ~ 1 , E >= 3 & E < 6 ~ 1e+03, E >= 6         ~ 1e+06)
  SUFFIX <- case_when(E <  3         ~ "", E >= 3 & E < 6 ~ "K"  , E >= 6         ~ "M"  )
  
  toPlot <- mutate(toPlot, Y_VAL = val/DIVIDE)

  min_date <- min(toPlot$DATE, na.rm = TRUE)
  max_date <- max(toPlot$DATE, na.rm = TRUE)

  x_breaks <- scales::pretty_breaks(n= 6)(toPlot$DATE)
  x_label_fmt <- ifelse(length(x_breaks) > length(unique(format(x_breaks, "%Y-%m"))), "%b %d\n%Y", "%b\n%Y")

  #for some reason need to call {ggplot2} before function - DON'T KNOW WHY, don't need to for plot_map.R 
  p <- ggplot2::ggplot(toPlot, ggplot2::aes(x = DATE, y = val/DIVIDE)) +
    ggplot2::geom_line() +
    ggplot2::geom_area(alpha = 0.6) +
    ggplot2::scale_x_date(name = NULL, date_labels = x_label_fmt, expand = c(0, 0)) +
    which_Y_scale(TYPE, SUFFIX) +
    ggplot2::labs(title = plottitle) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(t=3, r=0, b=6, l=0), family = "Ubuntu")
      , axis.text  = element_text(family = "Ubuntu Mono", size = 8) #default size ~ 8.9
      , axis.title = element_text(family = "Ubuntu")
      , axis.line  = element_line(color = "grey30")
      , axis.ticks = element_line(color = "grey30")
      , axis.ticks.length = unit(5, "pt")
      , plot.margin = margin(t=5.5, r=15, b=5.5, l=5.5, unit = "pt") #default theme_minimal()$plot.margin
    )
  
  logger::log_info(glue("Create Chart Map for {VAR}: {GEO}"))
  return(p)
  
}


