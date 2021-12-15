# Set global variables into list and save in data directory to be available in the app ----
plotly_presets <- list()

# Colours ----
plotly_presets$colours <- list(
  blues = list(
    blue_21 = '#003E6B', # dark blue
    blue_29 = '#4098D7',
    blue_33 = '#0A558C',
    blue_39 = '#62B0E8',
    blue_45 = '#0F609B',
    blue_55 = '#84C5F4',
    blue_65 = '#186FAF',
    blue_74 = '#B6E0FE',
    blue_85 = '#2680C2',
    blue_92 = '#DCEEFB'  # light blue
  ),
  yellows = list(
    yellow_30 = '#8D2B0B', # dark brown
    yellow_39 = '#F7C948',
    yellow_44 = '#B44D12',
    yellow_49 = '#FADB5F',
    yellow_55 = '#CB6E17',
    yellow_63 = '#FCE588',
    yellow_68 = '#DE911D',
    yellow_76 = '#FFF3C4',
    yellow_88 = '#F0B429',
    yellow_96 = '#FFFBEA'  # light yellow
  ),
  greys = list(
    grey_16 = '#102A43', # dark grey
    grey_23 = '#829AB1',
    grey_30 = '#243B53',
    grey_39 = '#9FB3C8',
    grey_49 = '#334E68',
    grey_60 = '#BCCCDC',
    grey_70 = '#486581',
    grey_80 = '#D9E2EC',
    grey_89 = '#627D98',
    grey_96 = '#F0F4F8'  # light grey
  ),
  cyans = list(
    cyan_17 = '#044E54', # dark cyan
    cyan_25 = '#0A6C74',
    cyan_29 = '#0E7C86',
    cyan_34 = '#14919B',
    cyan_45 = '#2CB1BC',
    cyan_50 = '#38BEC9',
    cyan_59 = '#54D1DB',
    cyan_75 = '#87EAF2',
    cyan_87 = '#BEF8FD',
    cyan_94 = '#E0FCFF'  # light cyan
  ),
  reds = list(
    red_20 = '#610404', # dark red
    red_25 = '#780A0A',
    red_32 = '#911111',
    red_38 = '#A61B1B',
    red_44 = '#BA2525',
    red_55 = '#D64545',
    red_66 = '#E66A6A',
    red_78 = '#F29B9B',
    red_89 = '#FACDCD',
    red_97 = '#FFEEEE'  # light red
  )
)

# Fonts ----
font_family <- "mono"
plotly_presets$font$family <- font_family

# Axes ticks ----
plotly_presets$tick$font <- list(family = font_family, size = 13)

plotly_presets$tick$format$string <- '%{y}'
plotly_presets$tick$format$integer <- ','
plotly_presets$tick$format$currency <- '$,.1f'
plotly_presets$tick$format$percentage <- '.0%f'
plotly_presets$tick$format$date <- "%B<br>%Y"

plotly_presets$tick$exponent_format = 'B'

# Axes format
plotly_presets$axis_x <- list(
  title = "", 
  showticklabels = TRUE, 
  tickfont = plotly_presets$tick$font, 
  color = plotly_presets$colour_palette$greys$grey_30, 
  tickformat = plotly_presets$tick$format
)

plotly_presets$axis_y <- list(
  title = "",
  showticklabels = TRUE,
  tickfont = plotly_presets$tick$font, 
  color = plotly_presets$colour_palette$greys$grey_30, 
  tickformat = plotly_presets$tick$format,
  rangemode = 'tozero'
)

# Margins ----
plotly_presets$margin_format <- list(
  l = 20,
  r = 20,
  b = 10,
  t = 10,
  pad = 5
)

# Legend ----
plotly_presets$legend_format <- list(
  font = list(
    family = font_family,
    size = 11,
    color = "#393939"
  ),
  bordercolor = "#e6e6e6",
  borderwidth = 1,
  bgcolor = "rgba(255, 255, 255, 0.5)",
  orientation = 'h',
  traceorder = 'normal'
)

plotly_presets$legend_format$bottom <- plotly_presets$legend_format
plotly_presets$legend_format$bottom$x <- 0
plotly_presets$legend_format$bottom$y <- -0.2
  
plotly_presets$legend_format$right <- plotly_presets$legend_format
plotly_presets$legend_format$right$x <- 1.2
plotly_presets$legend_format$right$y <- 0

plotly_presets$remove_buttons <- c(
  "zoom2d", "select2d", "lasso2d", "hoverClosestCartesian", "zoomIn3d",
  "zoomOut3d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian"
)

rm(font_family)
