# Builds the writing-by-outlet-and-year chart as an inline SVG partial.
#
# Form: a unit dot plot. One dot per published piece, positioned by year (x)
# and outlet (row). Position already encodes outlet identity, so color would be
# redundant: the chart is single-series and therefore carries no legend, per the
# dataviz rule that a single series needs none.
#
# Rows are ordered by first publication year, so the chart reads as a career
# arc rather than a ranking. Row heights vary with each row's busiest year to
# keep the figure compact.
#
# The 41-item list below the chart on the same page is its table view, so no
# value is reachable only by hover.
#
# Regenerate with: Rscript viz/build-writing-chart.R

suppressPackageStartupMessages({ library(yaml); library(dplyr) })

items <- yaml::read_yaml("data/writing.yml")
df <- tibble(
  outlet = vapply(items, function(x) x$outlet %||% NA_character_, ""),
  year   = vapply(items, function(x) as.integer(substr(as.character(x$date), 1, 4)), integer(1))
) |> filter(!is.na(outlet), !is.na(year))

yr <- range(df$year); years <- yr[1]:yr[2]
ord <- df |> group_by(outlet) |> summarise(first = min(year), n = n()) |> arrange(first, desc(n))

W <- 620; RIGHT <- 12; LABEL_H <- 15; UNIT <- 10; DOT_R <- 4; GAP <- 22; AXIS_H <- 24; TOPPAD <- 6
band <- (W - RIGHT) / length(years)
xc <- function(y) round(RIGHT/2 + band * (y - yr[1]) + band/2, 1)

rows <- list(); ytop <- TOPPAD
for (i in seq_len(nrow(ord))) {
  o <- ord$outlet[i]
  mx <- df |> filter(outlet == o) |> count(year) |> pull(n) |> max()
  h  <- mx * UNIT
  rows[[o]] <- list(label_y = ytop + LABEL_H - 6, base = ytop + LABEL_H + h, h = h)
  ytop <- ytop + LABEL_H + h + GAP
}
PLOT_H <- ytop - GAP
H <- PLOT_H + AXIS_H

grid <- paste0(sprintf('<line class="viz-grid" x1="%s" y1="0" x2="%s" y2="%s"/>',
                       xc(years), xc(years), PLOT_H), collapse = "")
ticks <- paste0(sprintf('<text class="viz-tick" x="%s" y="%s" text-anchor="middle">%s</text>',
                        xc(years), PLOT_H + 16, years), collapse = "")

parts <- c()
for (o in ord$outlet) {
  r <- rows[[o]]
  n_o <- ord$n[ord$outlet == o]
  parts <- c(parts, sprintf(
    '<text class="viz-row-label" x="%s" y="%s">%s <tspan class="viz-row-n">%d</tspan></text>',
    RIGHT/2, r$label_y, o, n_o))
  cnt <- df |> filter(outlet == o) |> count(year)
  for (k in seq_len(nrow(cnt))) {
    y0 <- cnt$year[k]; n <- cnt$n[k]
    for (j in seq_len(n)) {
      cy <- r$base - (j - 1) * UNIT - DOT_R - 1
      parts <- c(parts, sprintf(
        '<g class="viz-pt"><title>%s, %d</title><circle class="viz-hit" cx="%s" cy="%s" r="11"/><circle class="viz-dot" cx="%s" cy="%s" r="%s"/></g>',
        o, y0, xc(y0), cy, xc(y0), cy, DOT_R))
    }
  }
}

svg <- sprintf('```{=html}
<figure class="viz viz-scroll">
<svg viewBox="0 0 %d %d" role="img" aria-labelledby="w-title w-desc" class="viz-writing">
<title id="w-title">Published writing by outlet and year</title>
<desc id="w-desc">A unit dot plot. Each dot is one published piece, placed by year along the horizontal axis and grouped into a row per outlet, with rows ordered by first publication year. The full list of pieces follows below.</desc>
%s
%s
%s
</svg>
<figcaption>One dot per piece, %d in all, %d to %d. Rows are ordered by first publication.</figcaption>
</figure>
```
', W, H, grid, paste(parts, collapse = "\n"), ticks, nrow(df), yr[1], yr[2])

writeLines(svg, "_viz/writing-chart.md")
cat(sprintf("wrote _viz/writing-chart.md  viewBox %dx%d  dots=%d\n", W, H, nrow(df)))
print(ord)
