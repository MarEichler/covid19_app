
box::use(
    dplyr[...]
  , ggplot2[...]
  , glue[glue]
  , scales[percent, comma]
  , ./meta
)


#' Determine which scaling to use 
#' @param TYPE 
#' @param SUFFIX 
which_Y_scale <- function(TYPE, SUFFIX){
  if (TYPE == "pc")                 {out<-lst(scale_y_continuous(name=NULL, label=scales::percent_format()             , expand=c(0, 0)))}
  if (TYPE %in% c("count", "p100k")){out<-lst(scale_y_continuous(name=NULL, label=scales::comma_format(suffix = SUFFIX), expand=c(0, 0)))}
  return(out)
}


#' Create Line chart 
#' @param DT 
#' @param VAR 
#' @export
create_chart <- function(DT, VAR, GEO = "United States", DATE_RNG){
  
  toPlot    <- select(filter(DT, state_name == GEO, DATE >= DATE_RNG[1], DATE <= DATE_RNG[2]), DATE, val = all_of(VAR), state_name)
  plottitle <- glue("{meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$NAME}: {GEO}")
  
  TYPE   <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$TYPE
  ACC    <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$ACCURACY 
  
  y_breaks <- scales::pretty_breaks(n= 6)(toPlot$val)
  E <- round(mean(log(y_breaks[which(y_breaks != 0)], base = 10))-1, digits = 0)
  DIVIDE <- case_when(E <  3         ~ 1 , E >= 3 & E < 6 ~ 1e+03, E >= 6         ~ 1e+06)
  SUFFIX <- case_when(E <  3         ~ "", E >= 3 & E < 6 ~ "K"  , E >= 6         ~ "M"  )
  
  x_breaks <- scales::pretty_breaks(n= 6)(toPlot$DATE)
  x_label_fmt <- ifelse(length(x_breaks) > length(unique(format(x_breaks, "%Y-%m"))), "%b %d\n%Y", "%b %d\n%Y")
  
  p <- ggplot(toPlot, aes(x = DATE, y = val/DIVIDE)) + 
    geom_line() + 
    geom_area(alpha = 0.6) + 
    scale_x_date(name = NULL, date_labels = x_label_fmt, expand = c(0, 0)) + 
    which_Y_scale(TYPE, SUFFIX) + 
    labs(title = plottitle) + 
    theme_minimal() + 
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(t=3, r=0, b=6, l=0), family = "Ubuntu")
      , legend.spacing.x = unit(0, 'cm')
      , axis.text  = element_text(family = "Ubuntu Mono", size = 8) #default size ~ 8.9
      , axis.title = element_text(family = "Ubuntu")
      , axis.line  = element_line(color = "grey30")
      , axis.ticks = element_line(color = "grey30")
      , axis.ticks.length = unit(5, "pt")
    )
  
  return(p)
}


