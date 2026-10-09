# Ridgeline ("joyplot") of subject matter over time, for the Research page.
#
# Overlapping kernel densities, one ridge per subject, drawn back to front so
# the newer work overlaps the older. Ridges share a vertical scale, so height
# is comparable across rows and reflects items per year, not a normalised shape.
#
# Colour is an EMPHASIS treatment, not a ramp: every ridge is hatched in ink
# and only the current line of work carries the survey red. That keeps the
# chart to one accent and puts the weight on what is live now. "Other" is the
# folded tail and is drawn as an outline with no fill.
#
# TOPIC ASSIGNMENT IS EDITORIAL. The rules below are explicit so they can be
# corrected; anything unmatched lands in "Other" and is reported on stderr.
#
# Regenerate with: Rscript viz/build-topics.R

suppressPackageStartupMessages({ library(yaml); library(dplyr) })

# Bare & is invalid XML. Browsers tolerate it in inline SVG, but anything that
# parses the markup strictly (standalone .svg, a linter) will choke.
esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

topic_of <- function(title) {
  t <- tolower(title)
  if (grepl("religio|muslim|christian|gender gap|unaffiliated|sub-saharan|church|faith|interfaith|shall the religious", t)) return("Religion and global demography")
  if (grepl("marriage|marry|assortative|hooking|dating|child marriage|crossing boundaries|opportunities to meet", t)) return("Marriage, family, and education")
  if (grepl("debt|evict|garnish|court|lawsuit|pocket filing|housing|indigent defense|sued", t)) return("Courts, debt, and housing")
  if (grepl("census|voter|voting|poll|turnout|hard to count|vote your way", t)) return("Civic engagement")
  if (grepl("school|student|child|chip|trafficking|food program|academic performance|harvey keeps", t)) return("Children and education policy")
  return("Other")
}

w <- yaml::read_yaml("data/writing.yml")
p <- yaml::read_yaml("data/publications.yml")
df <- bind_rows(
  tibble(title = vapply(w, function(x) x$title, ""),
         year  = vapply(w, function(x) as.integer(substr(as.character(x$date), 1, 4)), integer(1))),
  tibble(title = vapply(p, function(x) x$title, ""),
         year  = vapply(p, function(x) as.integer(x$year), integer(1)))
) |> filter(!is.na(year)) |> mutate(topic = vapply(title, topic_of, ""))

yr <- range(df$year)
first_yr <- df |> group_by(topic) |> summarise(f = min(year), n = n()) |> arrange(f)
panels <- c(setdiff(first_yr$topic, "Other"), intersect("Other", first_yr$topic))

# Short display labels: the full topic names overrun the gutter at this width.
short <- c(
  "Religion and global demography"  = "Religion & demography",
  "Marriage, family, and education" = "Marriage & family",
  "Children and education policy"   = "Children & schools",
  "Civic engagement"                = "Civic engagement",
  "Courts, debt, and housing"       = "Courts, debt & housing",
  "Other"                           = "Other"
)

W <- 640; GUT <- 166; RPAD <- 10; ROW <- 36; TOP <- 16; AXIS_H <- 26
RIDGE <- 64                     # peak height; > ROW is what makes them overlap
# x1 runs past the last year so the newest ridge can taper instead of being cut
x0 <- yr[1] - 1.2; x1 <- yr[2] + 2.4
plotw <- W - GUT - RPAD
px <- function(v) round(GUT + plotw * (v - x0) / (x1 - x0), 1)

# common vertical scale across ridges
dens <- lapply(panels, function(tp) {
  v <- df$year[df$topic == tp]
  d <- density(v, bw = 0.95, from = x0, to = x1, n = 220)
  list(topic = tp, x = d$x, y = d$y * length(v))
})
ymax <- max(vapply(dens, function(d) max(d$y), 0))

parts <- c()
for (i in seq_along(dens)) {
  d <- dens[[i]]
  base <- TOP + (i - 1) * ROW + RIDGE
  yy   <- base - round(RIDGE * d$y / ymax, 2)
  pts  <- paste(sprintf("%s %s", px(d$x), yy), collapse = "L")
  cls  <- if (d$topic == "Other") "viz-ridge-other"
          else if (d$topic == "Courts, debt, and housing") "viz-ridge viz-ridge-mark"
          else "viz-ridge"
  fil  <- if (d$topic == "Other") "none"
          else if (d$topic == "Courts, debt, and housing") "url(#hatch-red)"
          else "url(#hatch-ink)"
  n_tp <- sum(df$topic == d$topic)
  parts <- c(parts, sprintf(
    paste0('<g class="viz-pt"><title>%s: %d items, %d to %d</title>',
           '<path class="%s" fill="%s" d="M%s %sL%sL%s %sZ"/>',
           '<text class="viz-ridge-label" x="%s" y="%s" text-anchor="end">%s <tspan class="viz-row-n">%d</tspan></text></g>'),
    esc(d$topic), n_tp, min(df$year[df$topic == d$topic]), max(df$year[df$topic == d$topic]),
    cls, fil, px(x0), base, pts, px(x1), base,
    GUT - 12, base - 3, esc(short[[d$topic]]), n_tp))
}

PLOT_H <- TOP + (length(dens) - 1) * ROW + RIDGE
H <- PLOT_H + AXIS_H
lab_years <- seq(yr[1] + 1, yr[2], by = 3)
ticks <- paste0(sprintf('<text class="viz-tick" x="%s" y="%s" text-anchor="middle">%s</text>',
                        px(lab_years), PLOT_H + 17, lab_years), collapse = "")
axis <- sprintf('<line class="viz-axis" x1="%s" y1="%s" x2="%s" y2="%s"/>',
                px(x0), PLOT_H, px(x1), PLOT_H)

svg <- sprintf('```{=html}
<figure class="viz viz-scroll">
<svg viewBox="0 0 %d %d" role="img" aria-labelledby="tp-t tp-d" class="viz-topics">
<defs>
<pattern id="hatch-ink" width="5" height="5" patternTransform="rotate(45)" patternUnits="userSpaceOnUse"><rect class="hatch-bg" width="5" height="5"/><line class="hatch-ink-line" x1="0" y1="0" x2="0" y2="5"/></pattern>
<pattern id="hatch-red" width="4" height="4" patternTransform="rotate(45)" patternUnits="userSpaceOnUse"><rect class="hatch-bg" width="4" height="4"/><line class="hatch-red-line" x1="0" y1="0" x2="0" y2="4"/></pattern>
</defs>
<title id="tp-t">What I have worked on, by year</title>
<desc id="tp-d">A ridgeline chart. Six overlapping density curves, one per subject area, sharing a year axis and a common vertical scale. Religious demography and family research peak around 2016 and fade after 2018. Courts, debt and housing begins in 2021 and rises to the present.</desc>
%s
%s
%s
</svg>
<figcaption>Each ridge is one subject area; height is items per year on a shared scale. %d items, %d to %d.</figcaption>
</figure>
```
', W, H, paste(parts, collapse = "\n"), axis, ticks, nrow(df), yr[1], yr[2])

writeLines(svg, "_viz/topics.md")
cat(sprintf("wrote _viz/topics.md  %d ridges, peak %.2f items/yr\n", length(dens), ymax))
for (tp in panels) cat(sprintf("  %-34s n=%d\n", tp, sum(df$topic == tp)))
