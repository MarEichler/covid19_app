

box::use(
    ggplot2[...]
  , magrittr[`%>%`]
  , dplyr[...]
  , ./plot_admin
)

shp_hex_centers <- readRDS("./data/shp_hex_centers.RDS") 
shp_hex         <- readRDS("./data/shp_hex.RDS")  

DT <- stateDT
VAR <- "C_CUM"



coldf <- plot_admin$coldf(VAR, VALS = DT[which(state_name != "United States"),][[VAR]])

toPlot <- DT %>% 
  filter(DATE == max(DATE)) %>%
  select(DATE, val = VAR, state_name) %>% 
  mutate(
      plotval  = plot_admin$plotval(val, VAR)
    , colgroup = cut(plotval, coldf$breaks[[1]], coldf$labels[[1]], right = FALSE)
    , font     = plot_admin$fontcol(colgroup, coldf)
    , display  = plot_admin$displayval(plotval, "comma")
  ) %>% 
  mutate(
    USA_val  = paste0("National US value: ", .[which(.$state_name == "United States"),]$display)
  ) 


USA_colgroup <- cut(toPlot[which(toPlot$state_name == "United States"),]$plotval, coldf$breaks[[1]], coldf$labels[[1]], right = FALSE)
USA_col      <- ifelse(is.na(USA_colgroup), "white", coldf$colors[[1]][which(coldf$labels[[1]] == USA_colgroup)])
USA_font     <- ifelse(is.na(USA_colgroup), "black", coldf$font  [[1]][which(coldf$labels[[1]] == USA_colgroup)]) 

plottitle <- plot_admin$plottitle(VAR, toPlot$DATE[1])
toPlot_centers <- merge(shp_hex_centers, toPlot)
toPlot_shp     <- merge(shp_hex        , toPlot)

ggplot(data = toPlot_shp) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = colgroup), color = "white" ) +
  geom_text(
      data = toPlot_centers
    , aes(x=x, y=y+5, label=state_abb)
    , color = toPlot_centers$font
    , fontface = "bold"
  ) +
  geom_text(
    data = toPlot_centers
    , aes(x=x, y=y-6, label=display)
    , color = toPlot_centers$font
    , size = 2.5
  )+
  facet_grid( . ~ USA_val) + 
  scale_fill_manual(
      name = NULL
    , values = coldf$colors[[1]]
    , drop = FALSE #show all categories
    , guide = guide_legend(label.position = "bottom", label.hjust = 0.5, keywidth = 6)
  ) +
  coord_fixed() + 
  theme_void() + 
  labs(title = plottitle) + 
  theme(
      plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(0, 0, 10, 0))
    , plot.subtitle = element_text(hjust = 0.5, size = 5)
    , strip.text.x  = element_text(size = 12, color = USA_font, face = "bold", margin = margin(3, 0, 3, 0))
    , strip.background.x = element_rect(fill = USA_col, color = USA_col)
    , legend.spacing.x = unit(0, 'cm')
    , legend.margin=margin(t = .5, unit='cm')
    , legend.position = "bottom"
  )


