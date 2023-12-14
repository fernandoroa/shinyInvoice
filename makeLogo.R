# sudo apt install libmagick++-dev
# install.packages("ggimage")
# # sudo apt-get install libfreetype6-dev
# install.packages("hexSticker")

library(hexSticker)

{
  xbase <- 0
  ybase <- 0
  xsize <- .8
  ysize <- .2
  ysizebig <- ysize * 2

  fpolygonx <- c(xbase, xsize, xsize, xbase)
  spolygonxorig <- fpolygonx + xsize * (1 / .8)
  spolygonx <- c(0, 2.8, 2.8, 0)
  tpolygonx <- spolygonxorig + xsize * (1 / .8)

  fpolygony <- c(ybase, ybase, ybase + ysize, ybase + ysize)

  spolygony1 <- c(0.375, 0.375, 0.4, 0.4)
  spolygony2 <- c(0.5, 0.5, 0.6, 0.6)
  spolygony3 <- c(0.7, 0.7, 0.8, 0.8)
  spolygony4 <- c(0.9, 0.9, 0.925, .925)

  spolygonyorig <- fpolygony + ysizebig * 2
  tpolygony <- spolygonyorig + ysize * 1.5

  xlist <- c(
    list(fpolygonx),
    rep(list(spolygonx), 4),
    list(tpolygonx)
  )

  ylist <- list(
    fpolygony,
    spolygony1, spolygony2,
    spolygony3,
    spolygony4,
    tpolygony
  )

  color2 <- "cadetblue4"
  color <- "azure4"
}

{
  plot("", xlim = c(0, 4), ylim = c(0, 2.5), axes = F, main = NA, xlab = "", ylab = "")
  mapply(function(x, y) polygon(x, y, col = "blue", border = NA), x = xlist, y = ylist)
}

{
  library(hexSticker)

  sticker(
    expression({
      plot("",
        xlim = c(0, 4), ylim = c(0, 1.5),
        axes = FALSE, main = NA,
        xlab = "", ylab = ""
      )
      mapply(function(x, y) polygon(x, y, col = color, border = NA),
        x = xlist, y = ylist
      )
    }),
    package = "shinyInvoice",
    p_size = 14, p_y = 1.5,
    s_x = 1.2, s_y = .8,
    s_width = 1.6, s_height = 1.5,
    h_color = color2,
    p_color = color2,
    h_fill = "white",
    filename = "logo.png"
  )
}
