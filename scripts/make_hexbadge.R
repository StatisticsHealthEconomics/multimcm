library(hexSticker)

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("lobster")
## Automatically use showtext to render text for future devices
showtext_auto()

sticker(
  "man/figures/cocktail.png",
  package = "multimcm",
  p_size = 25,   # png
  # p_size = 9,   # svg
  s_x = 1,
  s_y = 0.8,
  s_width = 0.5,
  # filename = "man/figures/hexbadge.svg",
  filename = "man/figures/hexbadge.png",
  h_fill = "lightblue",
  h_color = "darkgreen",
  p_y = 1.5,
  p_color = "darkgreen",
  p_family = "lobster",
  spotlight = FALSE,
  url = "github.com/StatisticsHealthEconomics/multimcm",
  u_size = 3.2,
  # u_size = 1.05,  # svg
  u_y = 0.05,
  l_alpha = 1,
  l_y = 0.85)
