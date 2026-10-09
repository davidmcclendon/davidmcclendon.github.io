# Builds the "places I have lived" map as an inline SVG partial.
#
# Geometry: US state boundaries from the `maps` package (bundled, offline),
# projected to EPSG:5070 (Albers equal area, CONUS) so areas are not distorted.
# Output: _viz/places-map.md, a raw-HTML block that Quarto includes inline, so
# the SVG inherits theme colors from CSS custom properties.
#
# Label placement is explicit because three pairs collide at national scale:
# Houston/Galveston sit ~7px apart and Richmond/Washington ~21px apart. Each
# label is anchored by a leader line rather than nudged off its dot.
#
# Regenerate with: Rscript viz/build-map.R

suppressPackageStartupMessages({ library(sf); library(maps); library(dplyr) })

W <- 742; H <- 405; PAD <- 8
RIGHT_LABEL <- 100   # reserved gutter: Washington and Richmond label to the right

states <- maps::map("state", plot = FALSE, fill = TRUE) |>
  st_as_sf() |> st_transform(5070) |> st_simplify(dTolerance = 9000)

places <- tribble(
  ~city,         ~state, ~lon,     ~lat,
  "Richmond",    "VA",   -77.4360, 37.5407,
  "Washington",  "DC",   -77.0369, 38.9072,
  "Austin",      "TX",   -97.7431, 30.2672,
  "Houston",     "TX",   -95.3698, 29.7604,
  "Galveston",   "TX",   -94.7977, 29.3013,
  "Minneapolis", "MN",   -93.2650, 44.9778
) |> st_as_sf(coords = c("lon", "lat"), crs = 4326) |> st_transform(5070)

bb <- st_bbox(states)
MAP_W <- W - PAD - RIGHT_LABEL
s  <- min((MAP_W - PAD) / (bb$xmax - bb$xmin), (H - 2*PAD) / (bb$ymax - bb$ymin))
ox <- PAD + ((MAP_W - PAD) - s * (bb$xmax - bb$xmin)) / 2
oy <- PAD + ((H - 2*PAD) - s * (bb$ymax - bb$ymin)) / 2
px <- function(x) round(ox + s * (x - bb$xmin), 1)
py <- function(y) round(oy + s * (bb$ymax - y), 1)

ring_d <- function(m) paste0("M", paste(sprintf("%s %s", px(m[,1]), py(m[,2])), collapse="L"), "Z")
geom_d <- function(g) {
  if (inherits(g, "POLYGON")) paste(vapply(g, ring_d, ""), collapse="")
  else if (inherits(g, "MULTIPOLYGON")) paste(vapply(unlist(g, recursive=FALSE), ring_d, ""), collapse="")
  else ""
}
d_all <- paste(vapply(st_geometry(states), geom_d, ""), collapse="")

pts <- st_coordinates(places)
p <- places |> st_drop_geometry() |> mutate(x = px(pts[,1]), y = py(pts[,2]))

# label dx, dy and text-anchor, chosen to clear the three colliding pairs
lab <- tribble(
  ~city,         ~dx,  ~dy, ~anchor,
  "Minneapolis",   9,  -7,  "start",
  "Washington",   13,  -6,  "start",
  "Richmond",     13,  11,  "start",
  "Austin",      -10,  -5,  "end",
  "Houston",      11,  -8,  "start",
  "Galveston",    13,  17,  "start"
)
p <- p |> left_join(lab, by = "city")

dots <- paste0(
  sprintf(
    '<g class="viz-pt"><title>%s, %s</title><line class="viz-leader" x1="%s" y1="%s" x2="%s" y2="%s"/><circle class="viz-hit" cx="%s" cy="%s" r="12"/><circle class="viz-dot" cx="%s" cy="%s" r="4.5"/><text class="viz-label" x="%s" y="%s" text-anchor="%s">%s</text></g>',
    p$city, p$state,
    p$x, p$y, p$x + p$dx * 0.72, p$y + p$dy * 0.72,
    p$x, p$y,
    p$x, p$y,
    p$x + p$dx, p$y + p$dy, p$anchor, p$city),
  collapse = "\n")

svg <- sprintf('```{=html}
<figure class="viz">
<svg viewBox="0 0 %d %d" role="img" aria-labelledby="map-title map-desc" class="viz-map">
<title id="map-title">Places I have lived</title>
<desc id="map-desc">A map of the United States marking six cities: Richmond, Virginia; Washington, D.C.; Austin, Houston and Galveston, Texas; and Minneapolis, Minnesota.</desc>
<path class="viz-geo" d="%s"/>
%s
</svg>
<figcaption>Richmond, Washington, Austin, Houston, Galveston, Minneapolis.</figcaption>
</figure>
```
', W, H, d_all, dots)

writeLines(svg, "_viz/places-map.md")
cat(sprintf("wrote _viz/places-map.md (%.1f KB)\n", file.size("_viz/places-map.md")/1024))
print(p)
