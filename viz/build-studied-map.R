# Map of the places the research covers, for the Writing page.
#
# Each highlighted state is tied to at least one named piece in data/writing.yml
# or data/publications.yml. Nothing here is inferred from topic alone.
#
# Regenerate with: Rscript viz/build-studied-map.R

suppressPackageStartupMessages({ library(sf); library(maps); library(dplyr) })

# state -> the work that covers it
studied <- c(
  "texas"         = "Harris County evictions, JP precinct caseloads, Galveston housing, Census 2020, Children at Risk statewide work",
  "minnesota"     = "Pocket filing in Minnesota; racial disparities in debt lawsuits",
  "wisconsin"     = "Consumer Debt Collection Lawsuits in Wisconsin, 2018-2024",
  "oregon"        = "Oregon debt collection findings chartbook; racial disparities",
  "michigan"      = "Racial disparities in consumer debt lawsuits",
  "california"    = "Written answer requirements and court participation",
  "missouri"      = "Consumer debt filings tracked, 2019-2025",
  "massachusetts" = "Consumer debt filings tracked, 2019-2025",
  "alabama"       = "Consumer debt filings tracked, 2019-2025",
  "utah"          = "Consumer debt filings tracked, 2019-2025",
  "north dakota"  = "Consumer debt filings tracked, 2019-2025",
  "virginia"      = "Consumer debt filings tracked, 2019-2025"
)

W <- 660; H <- 400; PAD <- 8

states <- maps::map("state", plot = FALSE, fill = TRUE) |>
  st_as_sf() |> st_transform(5070) |> st_simplify(dTolerance = 9000)
# maps::map names sub-regions as "state:part"; collapse to the state name
states$base <- sub(":.*$", "", states$ID)

bb <- st_bbox(states)
s  <- min((W - 2*PAD) / (bb$xmax - bb$xmin), (H - 2*PAD) / (bb$ymax - bb$ymin))
ox <- PAD + ((W - 2*PAD) - s * (bb$xmax - bb$xmin)) / 2
oy <- PAD + ((H - 2*PAD) - s * (bb$ymax - bb$ymin)) / 2
px <- function(x) round(ox + s * (x - bb$xmin), 1)
py <- function(y) round(oy + s * (bb$ymax - y), 1)

ring_d <- function(m) paste0("M", paste(sprintf("%s %s", px(m[,1]), py(m[,2])), collapse="L"), "Z")
geom_d <- function(g) {
  if (inherits(g, "POLYGON")) paste(vapply(g, ring_d, ""), collapse="")
  else if (inherits(g, "MULTIPOLYGON")) paste(vapply(unlist(g, recursive=FALSE), ring_d, ""), collapse="")
  else ""
}

off <- states |> filter(!base %in% names(studied))
d_off <- paste(vapply(st_geometry(off), geom_d, ""), collapse = "")

on_parts <- c()
for (nm in names(studied)) {
  g <- states |> filter(base == nm)
  if (nrow(g) == 0) { message("NO MATCH for state: ", nm); next }
  d <- paste(vapply(st_geometry(g), geom_d, ""), collapse = "")
  on_parts <- c(on_parts, sprintf(
    '<g class="viz-pt"><title>%s — %s</title><path class="viz-state-on" fill="url(#hatch-red)" d="%s"/></g>',
    tools::toTitleCase(nm), studied[[nm]], d))
}

svg <- sprintf('```{=html}
<figure class="viz">
<svg viewBox="0 0 %d %d" role="img" aria-labelledby="sm-t sm-d" class="viz-map">
<defs>
<pattern id="hatch-ink" width="5" height="5" patternTransform="rotate(45)" patternUnits="userSpaceOnUse"><rect class="hatch-bg" width="5" height="5"/><line class="hatch-ink-line" x1="0" y1="0" x2="0" y2="5"/></pattern>
<pattern id="hatch-red" width="4" height="4" patternTransform="rotate(45)" patternUnits="userSpaceOnUse"><rect class="hatch-bg" width="4" height="4"/><line class="hatch-red-line" x1="0" y1="0" x2="0" y2="4"/></pattern>
</defs>
<title id="sm-t">States my research covers</title>
<desc id="sm-d">A United States map with twelve states filled: Texas, Minnesota, Wisconsin, Oregon, Michigan, California, Missouri, Massachusetts, Alabama, Utah, North Dakota and Virginia. Each is tied to a named piece of published work.</desc>
<path class="viz-state-off" d="%s"/>
%s
</svg>
<figcaption>Alabama, California, Massachusetts, Michigan, Minnesota, Missouri, North Dakota, Oregon, Texas, Utah, Virginia, Wisconsin. Hover a state for the work it covers. The academic work reaches further: Ireland and Austria, Israel, sub-Saharan Africa, and religious demography worldwide.</figcaption>
</figure>
```
', W, H, d_off, paste(on_parts, collapse = "\n"))

writeLines(svg, "_viz/studied-map.md")
cat(sprintf("wrote _viz/studied-map.md (%.1f KB), %d states highlighted\n",
            file.size("_viz/studied-map.md")/1024, length(on_parts)))
