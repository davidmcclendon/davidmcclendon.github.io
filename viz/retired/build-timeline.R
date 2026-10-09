# Career timeline for the CV page: education milestones and employment spans
# on one axis.
#
# Dates come from CV_McClendonDM_JA.docx (Aug 2024). Cities are deliberately
# NOT a band here: reliable date ranges exist for Austin, Washington and
# Houston but not for Richmond or Galveston, and a band with guessed dates
# would be worse than no band.
#
# Regenerate with: Rscript viz/build-timeline.R

suppressPackageStartupMessages({ library(dplyr) })

NOW <- 2026.8

jobs <- tribble(
  ~role,                                        ~org,                       ~from,  ~to,
  "Partner and Principal Consultant",           "January Advisors",          2018,   NOW,
  "Research Associate",                         "UH Hobby School",           2018,   NOW,
  "Assistant then Co-Director of Research",     "CHILDREN AT RISK",          2017,   2018.5,
  "Research Associate",                         "Pew Research Center",       2015,   2017,
  "Statistical Consultant and Research Asst.",  "UT Austin PRC",             2012,   2014.8,
  "Teaching Assistant",                         "UT Austin",                 2010,   2011
) |> arrange(from)

# Short visible labels: the full degree names collide at this scale (three
# years apart is ~95px, and "B.A. Religious Studies" alone is wider than that).
# Full text lives in the tooltip, and the CV text below the figure spells it out.
degrees <- tribble(
  ~label,   ~full,                    ~year,  ~anchor,
  "B.A.",   "B.A. Religious Studies",  2008,  "start",
  "M.A.",   "M.A. Sociology",          2011,  "middle",
  "Ph.D.",  "Ph.D. Sociology",         2015,  "middle"
)

W <- 620; L <- 8; R <- 8; ROW <- 31; TOP <- 42; AXIS_PAD <- 30
x0 <- 2008; x1 <- 2027
px <- function(v) round(L + (W - L - R) * (v - x0) / (x1 - x0), 1)
H  <- TOP + nrow(jobs) * ROW + AXIS_PAD

gridyears <- seq(2008, 2026, by = 2)
grid <- paste0(sprintf('<line class="viz-grid" x1="%s" y1="%s" x2="%s" y2="%s"/>',
                       px(gridyears), 32, px(gridyears), TOP + nrow(jobs)*ROW - 2), collapse="")
ticks <- paste0(sprintf('<text class="viz-tick" x="%s" y="%s" text-anchor="middle">%s</text>',
                        px(gridyears), TOP + nrow(jobs)*ROW + 12, gridyears), collapse="")

degs <- paste0(sprintf(
  '<g class="viz-pt"><title>%s, %d</title><circle class="viz-hit" cx="%s" cy="24" r="12"/><circle class="viz-deg" cx="%s" cy="24" r="4.5"/><text class="viz-deg-label" x="%s" y="15" text-anchor="%s">%s</text></g>',
  degrees$full, degrees$year, px(degrees$year), px(degrees$year),
  px(degrees$year) + ifelse(degrees$anchor == "start", -4, 0), degrees$anchor, degrees$label), collapse="\n")

# Label sits ABOVE its bar and is left-aligned to the plot edge: a label set
# inside the bar would be clipped by the short spans (the 2010-11 assistantship
# is only ~30px wide at this scale).
bars <- vapply(seq_len(nrow(jobs)), function(i) {
  d <- jobs[i, ]; y <- TOP + (i - 1) * ROW
  ongoing <- d$to >= NOW
  sprintf(paste0(
    '<g class="viz-pt"><title>%s, %s</title>',
    '<text class="viz-span-label" x="%s" y="%s">%s <tspan class="viz-row-n">%s</tspan></text>',
    '<rect class="viz-hit" x="%s" y="%s" width="%s" height="%s"/>',
    '<rect class="viz-span%s" x="%s" y="%s" width="%s" height="10" rx="4"/></g>'),
    d$role, d$org,
    L, y + 9, d$org, d$role,
    px(d$from) - 4, y + 11, max(14, round(px(d$to) - px(d$from), 1)) + 8, 16,
    if (ongoing) " viz-span-now" else "",
    px(d$from), y + 14, max(6, round(px(d$to) - px(d$from), 1)))
}, "")

svg <- sprintf('```{=html}
<figure class="viz viz-scroll">
<svg viewBox="0 0 %d %d" role="img" aria-labelledby="tl-t tl-d" class="viz-timeline">
<title id="tl-t">Education and employment, 2008 to the present</title>
<desc id="tl-d">A timeline. Three degree milestones sit along the top: a B.A. in 2008, an M.A. in 2011 and a Ph.D. in 2015. Below them, horizontal bars mark seven positions, from a teaching assistantship in 2010 through January Advisors from 2018 to the present.</desc>
%s
%s
%s
%s
</svg>
<figcaption>Degrees above, positions below. Hover any bar for the full title.</figcaption>
</figure>
```
', W, H, grid, degs, paste(bars, collapse="\n"), ticks)

writeLines(svg, "_viz/timeline.md")
cat(sprintf("wrote _viz/timeline.md  %d positions, %d degrees\n", nrow(jobs), nrow(degrees)))
