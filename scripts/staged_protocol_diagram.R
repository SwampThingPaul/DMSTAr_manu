library(ggplot2)
library(grid)

protocol <- data.frame(
  stage = 1:6,
  title = c(
    "Specification alignment",
    "Controlled test construction",
    "Component-level verification",
    "Compositional verification",
    "Application-scale agreement",
    "Numerical behavior and reproducibility"
  ),
  detail = c(
    "Equations, states, units, logic, and versions",
    "Matched inputs, parameters, and settings",
    "States, fluxes, constraints, and edge cases",
    "Units, cases, and routed networks",
    "Trajectories and management metrics",
    "Resolution sensitivity, code, and benchmarks"
  ),
  dmstar = c(
    "DMSTA equations and version semantics",
    "Matched project inputs and configurations",
    "Hydrologic and P states, fluxes, and edge cases",
    "Cells, cases, splitters, and routed networks",
    "Daily and annual outputs across four applications",
    "Steps per day and archived benchmarks"
  ),
  fill = c(
    "#D9EAF3",
    "#CDE3E3",
    "#DCE8CC",
    "#F2E7BD",
    "#F1D5BE",
    "#E1D5E7"
  ),
  stringsAsFactors = FALSE
)

## Place Stage 1 at the top and Stage 6 at the bottom
protocol$x <- 1
protocol$y <- rev(seq_len(nrow(protocol)))

## Box dimensions
box_half_width  <- 1.5
box_half_height <- 0.4

protocol$xmin <- protocol$x - box_half_width
protocol$xmax <- protocol$x + box_half_width
protocol$ymin <- protocol$y - box_half_height
protocol$ymax <- protocol$y + box_half_height

## Downward arrows connecting consecutive stages
forward_arrows <- data.frame(
  x = 1,
  xend = 1,
  y = protocol$ymin[1:5] - 0.03,
  yend = protocol$ymax[2:6] + 0.03
)

p <- ggplot() +
    ## Downward arrows, drawn first so they remain behind the boxes
  geom_segment(data = forward_arrows,
    aes(x = x, xend = xend, y = y, yend = yend),
    linewidth = 0.55, colour = "#4D4D4D",
    arrow = arrow(type = "closed",length = unit(0.10, "inches"))) +
  ## Main stage boxes
  geom_rect(data = protocol,
    aes(xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax,fill = fill),
    colour = "#404040",linewidth = 0.45) +
  scale_fill_identity() +
  ## Stage-number circles along the left side of each box
  geom_point(data = protocol,aes(x = xmin + 0.23, y = y),
    shape = 21, size = 7.2, stroke = 0.4, fill = "#404040", colour = "#404040") +
  geom_text(data = protocol,aes(x = xmin + 0.23, y = y, label = stage),
    colour = "white", fontface = "bold", size = 3.5 ) +
  ## Stage titles
  geom_text(data = protocol,aes(x = xmin + 0.48, y = y + 0.19, label = title),
    hjust = 0,fontface = "bold",size = 3.45 ) +
  ## General verification description
  geom_text(data = protocol,aes(x = xmin + 0.48, y = y - 0.02, label = detail),
    hjust = 0, size = 2.95,  colour = "#303030" ) +
  ## DMSTAr-specific application
  geom_text(data = protocol,aes(x = xmin + 0.48, y = y - 0.21, label = dmstar),
    hjust = 0,  size = 2.75,  fontface = "italic",  colour = "#315A70") +
  ## Feedback path on the right side
  geom_segment( aes(x = protocol$xmax[6] + 0.10,xend = protocol$xmax[6] + 0.55,
                    y = protocol$y[6],yend = protocol$y[6]),
    linewidth = 0.45, colour = "#666666") +
  geom_segment(aes(x = protocol$xmax[6] + 0.55,xend = protocol$xmax[1] + 0.55,
                   y = protocol$y[6], yend = protocol$y[1]),
    linewidth = 0.45,colour = "#666666",
    arrow = arrow(type = "closed",length = unit(0.09, "inches"))) +
  geom_segment(aes(x = protocol$xmax[1] + 0.55,xend = protocol$xmax[1] + 0.10,
                   y = protocol$y[1],yend = protocol$y[1]),
    linewidth = 0.45,colour = "#666666") +
  ## Feedback label, rotated along the return path
  annotate("text",x = protocol$xmax[1] + 0.7,y = mean(range(protocol$y)),
    label = "Diagnose discrepancies, revise,\nand repeat",
    angle = 90, fontface = "italic", size = 2.9,lineheight = 0.95,colour = "#555555") +
  coord_cartesian(xlim = c(-1.05, 3.5),ylim = c(0.75, 6.1), clip = "off") +
  labs(
    title = "Staged verification of a modernized ecological model"
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold",size = 13,hjust = 0.5,margin = margin(b = 12)),
    plot.margin = margin( t = 12,r = 45,b = 6,l = 12))

p
ggsave(
  filename = "./figures/legacy_model_verification_protocol_ggplot.png",
  plot = p,
  width = 6,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


