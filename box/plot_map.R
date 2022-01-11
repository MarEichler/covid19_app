
box::use(
    dplyr[...]
  , ggplot2[...]
  , tibble[tribble]
  , glue[glue]
  , scales[percent, comma]
  , glue[glue]
  , stats[quantile]
  , ./meta
#doesn't work in box::use({package}[...]) 
#don't know why ? but need to load using library() otherwise error when upload to shinyapps.io 
  #, DescTools[RoundTo]
  #, logger[...]
)



#' Variable Type 
#' @param VAR Character(1, ... = "Not Valid VAR" ~ . %in% meta$VAROPTS$VAR)
vartype <-  function(VAR){
  TYPE <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR), ]$TYPE
  return(TYPE)
}


#' Plot Values (round for MA7)
#' @param VAL 
#' @param VAR Character(1, ... = "Not Valid VAR" ~ . %in% meta$VAROPTS$VAR)
plotval <- function(VAL , VAR){
  TYPE  <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$TYPE
  ACC   <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$ACCURACY 
  GROUP <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$GROUP 
  DIG   <- log(1/ACC, base = 10)
  
       if (TYPE == "p100k" & GROUP ==  "C" ){out <- ifelse(VAL <=  1.5, round(VAL, digits = DIG), round(VAL, digits = 1)) }
  else if (TYPE == "p100k" & GROUP !=  "C" ){out <-                     round(VAL, digits = DIG)  }
  else if (TYPE == "count"                 ){out <-                     round(VAL, digits = DIG)  }
  else if (TYPE == "pc"                    ){
    digp <- log(100/ACC, base = 10)
                                             out <-                     round(VAL, digits = digp) }
  return(out)
}


#' Color DF with breaks, labels, colors 
#' @param VAR 
#' @param VALS 
#' @param TYPE 
coldf <- function(VAR, VALS){
  TYPE <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR), ]$TYPE
  if (VAR %in% c("C_MA7_P100K", "C_NEW_P100K")){
  #SET BREAKS!!
    coldf <- tribble(
      ~breaks, ~labels, ~colors, ~font
      , c(-Inf, 1, 10, 25, 50, Inf) 
      , c("<1 Low" , "[1,10) Medium", "[10,25) High", "[25,50) Very High", "50+ Extremely High")
      , c("#51A09E", "#EFC637"      , "#E38D2C"     , "#A23520"          , "#87216B")
      , c("black"  ,  "black"       , "black"       , "white"            , "white")
    )
  } else {
  #BREAKS BASED ON QUANTILES!! 
    quant_perc <- c(0.2, 0.4, 0.6, 0.8)
    RNDVAL <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$RNDVAL 
    breaks <- c(0, DescTools::RoundTo(quantile(x = VALS, prob = quant_perc, na.rm = TRUE), RNDVAL), Inf)
    if (length(unique(breaks)) != 6){
      min <- max(DescTools::RoundTo(min(VALS - RNDVAL, na.rm = TRUE), RNDVAL), 0)
      max <-     DescTools::RoundTo(max(VALS + RNDVAL, na.rm = TRUE), RNDVAL)
      sec <- max/5
      breaks <- c(-Inf, min+sec, min+sec*2, min+sec*3, min+sec*4, Inf)
    }
    coldf <- tribble(
      ~breaks, ~labels, ~colors, ~font
      , breaks 
      , set_quant_labels(breaks, VAR)
      , c("#51A09E", "#EFC637"      , "#E38D2C"     , "#A23520"          , "#87216B")
      , c("black"  ,  "black"       , "black"       , "white"            , "white")
    )
  } #end if/else set breaks/quantile breaks 
  
  return(coldf)
  
}


#' Display Values 
#' @param VAL 
#' @param VAR Character(1)
displayval <-  function(VAL , VAR ){
  TYPE  <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR), ]$TYPE
  ACC   <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$ACCURACY 
  GROUP <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$GROUP 
  
       if (TYPE == "p100k" & GROUP ==  "C" ){out <- ifelse(VAL <= 1.5 | VAL != 0, comma(VAL, accuracy = 0.1), comma(VAL, accuracy = 1)) }
  else if (TYPE == "p100k" & GROUP !=  "C" ){out <- ifelse(             VAL != 0, comma(VAL, accuracy = ACC), comma(VAL, accuracy = 1)) }
  else if (TYPE == "count"                 ){out <-                               comma(VAL, accuracy = ACC) }
  else if (TYPE == "pc"                    ){
    out <- case_when(
        VAL == 0      ~ "0%"
      , VAL <  ACC/100 ~ paste0("<", percent(ACC/100, accuracy = ACC))
      , VAL >= ACC/100 ~ percent(VAL, accuracy = ACC) 
    )
    }
  return(out)
}


#' Set Labels for Breaks 
#' @param BREAKS 
#' @param TYPE 
set_quant_labels <- function(BREAKS, VAR){
  TYPE <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR), ]$TYPE
  if (TYPE == "count"){
    labels <- c(
        glue("<{comma(BREAKS[2])}")
      , glue( "{comma(BREAKS[2])} - {comma(BREAKS[3]-1)}")
      , glue( "{comma(BREAKS[3])} - {comma(BREAKS[4]-1)}")
      , glue( "{comma(BREAKS[4])} - {comma(BREAKS[5]-1)}")
      , glue( "{comma(BREAKS[5])}+")
    )
  } # end if (TYPE == "count")
  if (TYPE == "pc"){
    ACC <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$ACCURACY 
    labels <- c(
        glue("<{percent(BREAKS[2], accuracy = ACC)}")
      , glue( "{percent(BREAKS[2], accuracy = ACC)} - {percent(BREAKS[3]-ACC/100, accuracy = ACC)}")
      , glue( "{percent(BREAKS[3], accuracy = ACC)} - {percent(BREAKS[4]-ACC/100, accuracy = ACC)}")
      , glue( "{percent(BREAKS[4], accuracy = ACC)} - {percent(BREAKS[5]-ACC/100, accuracy = ACC)}")
      , glue( "{percent(BREAKS[5], accuracy = ACC)}+")
    )
  } #end if (TYPE == "pc)
  if (TYPE == "p100k"){
    ACC <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$ACCURACY 
    labels <- c(
        glue("<{comma(BREAKS[2], accuracy = ACC)}")
      , glue( "{comma(BREAKS[2], accuracy = ACC)} - {comma(BREAKS[3]-ACC, accuracy = ACC)}")
      , glue( "{comma(BREAKS[3], accuracy = ACC)} - {comma(BREAKS[4]-ACC, accuracy = ACC)}")
      , glue( "{comma(BREAKS[4], accuracy = ACC)} - {comma(BREAKS[5]-ACC, accuracy = ACC)}")
      , glue( "{comma(BREAKS[5], accuracy = ACC)}+")
    )
  } #end if (TYPE == "p100k)
  return(labels)
} #end function set_quant_labels 


#' Set Font Color 
#' @param COLGROUP 
#' @param COLDF 
fontcol <- function(COLGROUP, COLDF){
  case_when(
      COLGROUP == COLDF$labels[[1]][1] ~ COLDF$font[[1]][1]
    , COLGROUP == COLDF$labels[[1]][2] ~ COLDF$font[[1]][2]
    , COLGROUP == COLDF$labels[[1]][3] ~ COLDF$font[[1]][3]
    , COLGROUP == COLDF$labels[[1]][4] ~ COLDF$font[[1]][4]
    , COLGROUP == COLDF$labels[[1]][5] ~ COLDF$font[[1]][5]
  )
}


#' Title for Plot 
#' @param VAR 
#' @param VAL 
hextitle <- function(VAR, DATE){
  fmt_date <- format(DATE, "%b %d, %Y")
  prefix <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR),]$NAME
  title <- glue("{prefix} as of {fmt_date}")
  return(title)
}


#' Hex Banner Text Info (US Value) 
#' @param PLOTDF 
#' @param COLDF 
#' @param VAR 
hexbanner <- function(VAR, PLOTDF, COLDF){
  TYPE <- meta$VAROPTS[which(meta$VAROPTS$VAR == VAR), ]$TYPE
  if (TYPE == "count"){
    colgroup <- NA
    col      <- "white"
    font     <- "black"
  }
  if (TYPE %in% c("pc", "p100k")){
    colgroup <- cut(PLOTDF[which(PLOTDF$state_name == "United States"),]$plotval, COLDF$breaks[[1]], COLDF$labels[[1]], right = FALSE)
    col      <- COLDF$colors[[1]][which(COLDF$labels[[1]] == colgroup)]
    font     <- COLDF$font  [[1]][which(COLDF$labels[[1]] == colgroup)] 
  }
  out <- list(col, font)
  return(out)
}


#' Create Hex Map 
#' @param DT 
#' @param VAR 
#' @export
#' @examples
create_map <- function(DT, VAR){
  
  subset <- select(filter(DT, DATE == max(DATE)), DATE, val = all_of(VAR), state_name)
  
  coldf <- coldf(VAR, VALS = subset[which(state_name != "United States"),]$val)
  
  geom_text_size <- ifelse(VAR == "C_CUM", 2.25, 2.5)
  # 3.88 is default size, GeomText$default_aes$size
  
  
  toPlot <- mutate(subset
                   , plotval  = plotval(val, VAR)
                   , colgroup = cut(plotval, coldf$breaks[[1]], coldf$labels[[1]], right = FALSE)
                   , font     = fontcol(colgroup, coldf)
                   , display  = displayval(plotval, VAR)
  ) %>% 
    mutate(
      USA_val  = paste0("National US value: ", .[which(.$state_name == "United States"),]$display)
    ) 
  
  USA_col      <- hexbanner(VAR, toPlot, coldf)[[1]]
  USA_font     <- hexbanner(VAR, toPlot, coldf)[[2]]
  
  plottitle <- hextitle(VAR, toPlot$DATE[1])
  toPlot_centers <- merge(meta$shp_hex_centers, toPlot)
  toPlot_shp     <- merge(meta$shp_hex        , toPlot)
  
  p <- ggplot(data = toPlot_shp) + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = colgroup), color = "white" ) +
    geom_text(
      data = toPlot_centers
      , aes(x=x, y=y+5, label=state_abb)
      , color = toPlot_centers$font
      , fontface = "bold"
      , family  = "Ubuntu Mono"
    ) +
    geom_text(
      data = toPlot_centers
      , aes(x=x, y=y-6, label=display)
      , color = toPlot_centers$font
      , size = geom_text_size
      , family  = "Ubuntu Mono"
    )+
    facet_grid( . ~ USA_val) + 
    scale_fill_manual(
        name = NULL
      , values = coldf$colors[[1]]
      , drop = FALSE #show all categories
      , guide = guide_legend(label.position = "right", reverse = TRUE, label.hjust = 0, keywidth = 1.5)
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 5)) + 
    coord_fixed() + 
    theme_void() + 
    labs(title = plottitle) + 
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(t=3, r=0, b=6, l=0), family = "Ubuntu")
      , strip.text.x  = element_text(size = 12, color = USA_font, face = "bold", margin = margin(t=3, r=0, b=3, l=0), family = "Ubuntu")
      , strip.background.x = element_rect(fill = USA_col, color = USA_col)
      , legend.spacing.x = unit(0, 'cm')
      , legend.margin=margin(t=0, r=0, b=0, l=0) 
      , legend.text = element_text(margin = margin(t=0, r=0, b=0, l=1), family  = "Ubuntu Mono")
      , legend.position = "right"
      , plot.background  = element_rect(fill = "white", color = "white")
      , panel.background = element_rect(fill = "white", color = "white")
    )
  logger::log_info(glue("Create Hex Map for {VAR}"))
  return(p)
}


