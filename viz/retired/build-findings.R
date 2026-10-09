# Two charts drawn from published findings, for the Writing page.
#
# EVERY NUMBER BELOW IS QUOTED FROM THE LINKED POST. Nothing is estimated or
# interpolated. If a post is updated, re-check these against it.
#
# Regenerate with: Rscript viz/build-findings.R

suppressPackageStartupMessages({ library(dplyr) })

fmt <- function(x) formatC(x, format = "d", big.mark = ",")

# ---------------------------------------------------------------- finding 1
# Source: "Consumer Debt Filings Kept Climbing in 2025", January Advisors,
# 2026-07-15. Quoted: filings in 2025 as a share of each state's 2019 total.
filings <- tribble(
  ~state,           ~pct,
  "Missouri",        188,
  "Texas",           177,
  "Massachusetts",   153,
  "Alabama",         140,
  "Utah",            123,
  "North Dakota",    119,
  "Minnesota",       111,
  "Virginia",         98
) |> arrange(desc(pct))

W <- 620; GUT <- 104; RPAD <- 54; BAR_H <- 13; ROW <- 27; TOP <- 22
plotw <- W - GUT - RPAD
xmax  <- 200
sx    <- function(v) GUT + plotw * v / xmax
H1    <- TOP + nrow(filings) * ROW + 10

base_x <- sx(0); ref_x <- sx(100)
rows <- vapply(seq_len(nrow(filings)), function(i) {
  d <- filings[i, ]; y <- TOP + (i - 1) * ROW
  sprintf(paste0(
    '<g class="viz-pt"><title>%s: %d%% of its 2019 filings</title>',
    '<text class="viz-row-label viz-cat" x="%s" y="%s" text-anchor="end">%s</text>',
    '<rect class="viz-bar" x="%s" y="%s" width="%s" height="%s" rx="4"/>',
    '<rect class="viz-bar-foot" x="%s" y="%s" width="4" height="%s"/>',
    '<text class="viz-val" x="%s" y="%s">%d%%</text></g>'),
    d$state, d$pct,
    GUT - 10, y + BAR_H - 4, d$state,
    base_x, y, round(sx(d$pct) - base_x, 1), BAR_H,
    base_x, y, BAR_H,                      # square end at the baseline
    round(sx(d$pct), 1) + 7, y + BAR_H - 4, d$pct)
}, "")

svg1 <- sprintf('```{=html}
<figure class="viz viz-scroll">
<svg viewBox="0 0 %d %d" role="img" aria-labelledby="f1t f1d" class="viz-findings">
<title id="f1t">Consumer debt filings in 2025 as a share of each state\'s 2019 total</title>
<desc id="f1d">Horizontal bars for eight states. Missouri 188 percent, Texas 177, Massachusetts 153, Alabama 140, Utah 123, North Dakota 119, Minnesota 111, Virginia 98. A reference line marks the 2019 level at 100 percent.</desc>
<line class="viz-ref" x1="%s" y1="%s" x2="%s" y2="%s"/>
<text class="viz-ref-label" x="%s" y="%s" text-anchor="middle">2019 level</text>
%s
</svg>
<figcaption>Filings returned above pre-pandemic levels in seven of the eight states tracked. Source: <a href="https://www.januaryadvisors.com/consumer-debt-filings-kept-climbing-in-2025/">Consumer Debt Filings Kept Climbing in 2025</a>, with Divia Kallattil.</figcaption>
</figure>
```
', W, H1,
  ref_x, TOP - 12, ref_x, H1 - 14,
  ref_x, TOP - 17,
  paste(rows, collapse = "\n"))

writeLines(svg1, "_viz/finding-filings.md")

# ---------------------------------------------------------------- finding 2
# Source: "Redrawing Houston's Eviction Courts", January Advisors, 2022-11-11.
# Quoted: 1,000 simulated maps had a caseload standard deviation with a median
# of 7,838, ranging 3,750 to 10,683; the current map's is 39,157.
sim_lo <- 3750; sim_md <- 7838; sim_hi <- 10683; actual <- 39157

W2 <- 620; L <- 16; R <- 26; H2 <- 150
AXIS_Y <- 104; TRACK_Y <- 58
x2max <- 42000
px2 <- function(v) round(L + (W2 - L - R) * v / x2max, 1)

tickvals <- c(0, 10000, 20000, 30000, 40000)
ticks2 <- paste0(sprintf(
  '<line class="viz-grid" x1="%s" y1="%s" x2="%s" y2="%s"/><text class="viz-tick" x="%s" y="%s" text-anchor="middle">%s</text>',
  px2(tickvals), TRACK_Y - 26, px2(tickvals), AXIS_Y - 14, px2(tickvals), AXIS_Y, fmt(tickvals)), collapse = "")

svg2 <- sprintf('```{=html}
<figure class="viz viz-scroll">
<svg viewBox="0 0 %d %d" role="img" aria-labelledby="f2t f2d" class="viz-findings">
<title id="f2t">Caseload imbalance across Harris County justice of the peace precincts</title>
<desc id="f2d">A number line of caseload standard deviation. One thousand simulated precinct maps fall between 3,750 and 10,683, with a median of 7,838. The map actually in use sits at 39,157, far to the right of every simulated alternative.</desc>
%s
<g class="viz-pt"><title>1,000 simulated maps: 3,750 to 10,683, median 7,838</title>
<rect class="viz-band" x="%s" y="%s" width="%s" height="16" rx="4"/>
<line class="viz-median" x1="%s" y1="%s" x2="%s" y2="%s"/>
</g>
<text class="viz-anno" x="%s" y="%s" text-anchor="middle">1,000 simulated maps (3,750 to 10,683)</text>
<g class="viz-pt"><title>The map actually in use: 39,157</title>
<circle class="viz-hit" cx="%s" cy="%s" r="14"/>
<circle class="viz-dot viz-dot-alert" cx="%s" cy="%s" r="6"/>
</g>
<text class="viz-anno viz-anno-strong" x="%s" y="%s" text-anchor="end">Current map 39,157</text>
<text class="viz-axis-title" x="%s" y="%s">Standard deviation of caseloads across the eight precincts</text>
</svg>
<figcaption>A thousand alternative precinct maps were simulated. Every one of them balanced caseloads better than the map Harris County actually uses. Source: <a href="https://www.januaryadvisors.com/redrawing-houstons-eviction-courts-any-map-is-better-than-what-we-have-now/">Redrawing Houston\'s Eviction Courts</a>.</figcaption>
</figure>
```
', W2, H2, ticks2,
  px2(sim_lo), TRACK_Y, round(px2(sim_hi) - px2(sim_lo), 1),
  px2(sim_md), TRACK_Y - 5, px2(sim_md), TRACK_Y + 21,
  px2(sim_md), TRACK_Y - 13,
  px2(actual), TRACK_Y + 8, px2(actual), TRACK_Y + 8,
  px2(actual) - 12, TRACK_Y + 12,
  L, H2 - 16)

writeLines(svg2, "_viz/finding-courts.md")
cat("wrote _viz/finding-filings.md and _viz/finding-courts.md\n")
