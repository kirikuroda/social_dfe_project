# Shared colours, themes, and save helper for all visualization scripts.

library(tidyverse)
library(extrafont)

# Register installed system fonts (incl. Arial) with the base pdf device so
# save_pdf() can set family = "Arial". Fonts must be imported once beforehand
# via extrafont::font_import(pattern = "Arial").
suppressMessages(loadfonts(device = "pdf", quiet = TRUE))

out_dir <- here::here("output/figure")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Save helper: all figures saved as PDF with transparent background, using Arial
# via extrafont. Text is drawn with the base pdf device's default family
# (= "Arial"), then the font outlines are embedded with Ghostscript.
# width and height are specified in cm
save_pdf <- function(plot, filename, width, height) {
  path <- file.path(out_dir, filename)
  ggsave(
    path, plot = plot,
    width = width, height = height, units = "cm",
    bg = "transparent", device = "pdf"
  )
  embed_fonts(path)
}

# Colours
col_teal      <- "#298c8c"
col_yellow    <- "#ffb007"
col_grey      <- "#999999"
col_bg        <- "white"
col_dark      <- "#333333"
col_condition <- c(solo = col_grey, group = col_yellow)

# Shared themes
theme_transparent <- theme(
  plot.background  = element_rect(fill = "transparent", color = NA),
  panel.background = element_rect(fill = "transparent", color = NA)
)

theme_fig <- list(
  xlim(0, 40),
  guides(color = "none", shape = "none", fill = "none"),
  theme_minimal(base_size = 8),
  theme(
    axis.line        = element_line(color = col_dark),
    axis.text        = element_text(color = col_dark),
    panel.grid.minor = element_blank(),
    text             = element_text(color = col_dark),
    strip.text.x     = element_text(size = 8),
    strip.text.y     = element_blank()
  ),
  theme_transparent
)
