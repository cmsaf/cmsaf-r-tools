# Private functions taken from colorspace R package:
# YEAR: 2005-2018
# COPYRIGHT HOLDER: Ross Ihaka, Paul Murrell, Kurt Hornik, Jason C. Fisher, Reto Stauffer, Claus O. Wilke, Claire D. McWhite, Achim Zeileis
# ORGANIZATION: Ross Ihaka, Paul Murrell, Kurt Hornik, Jason C. Fisher, Reto Stauffer, Claus O. Wilke, Claire D. McWhite, Achim Zeileis

# -------------------------------------------------------------------
# Environment for passing around internal information
# -------------------------------------------------------------------

.colorspace_env <- new.env()

.colorspace_get_info <- function(x = NULL) {
  if (is.null(x))
    return(as.list(.colorspace_env))
  x <- as.character(x)[1L]
  return(.colorspace_env[[x]])
}

.colorspace_set_info <- function(...) {
  dots <- list(...)
  if (is.null(names(dots))) {
    stop("arguments must be named")
  } else if (any(names(dots) == "")) {
    warning("ignoring unnamed arguments")
    dots <- dots[names != ""]
  }
  if (length(dots) > 0L) {
    for (i in names(dots)) {
      .colorspace_env[[i]] <- dots[[i]]
    }
  }
  invisible(NULL)
}

.colorspace_set_info(
  hclwizard_autohclplot = FALSE,
  hclwizard_ninit       = 7,
  hclwizard_verbose     = FALSE,
  hclwizard_shiny.trace = FALSE
)

# -------------------------------------------------------------------
# Palette specifications
# -------------------------------------------------------------------

vars.pal <-
  c("h1", "h2", "c1", "c2", "l1", "l2", "p1", "p2", "cmax", "fixup")
# Inspired by:
qual.pals <- list()
qual.pals[["Pastel 1"]]    <-
  c(0,   NA,  35, NA, 85, NA,  NA,  NA,  NA, 1) # ColorBrewer.org: Pastel1
qual.pals[["Dark 2"]]      <-
  c(0,   NA,  50, NA, 60, NA,  NA,  NA,  NA, 1) # ColorBrewer.org: Dark2
qual.pals[["Dark 3"]]      <-
  c(0,   NA,  80, NA, 60, NA,  NA,  NA,  NA, 1) # JCF/Z: ~Dark2 with more chroma
qual.pals[["Set 2"]]       <-
  c(0,   NA,  60, NA, 70, NA,  NA,  NA,  NA, 1) # ColorBrewer.org: Set2
qual.pals[["Set 3"]]       <-
  c(10,   NA,  50, NA, 80, NA,  NA,  NA,  NA, 1) # ColorBrewer.org: Set3
qual.pals[["Warm"]]        <-
  c(90,-30,  50, NA, 70, NA,  NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Warm (based on Ihaka-03)
qual.pals[["Cold"]]        <-
  c(270,  150,  50, NA, 70, NA,  NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Cold (based on Ihaka-03)
qual.pals[["Harmonic"]]    <-
  c(60,  240,  50, NA, 70, NA,  NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Harmonic (based on Ihaka-03)
qual.pals[["Dynamic"]]     <-
  c(30,   NA,  50, NA, 70, NA,  NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Dynamic (based on Ihaka-03)

seqs.pals <- list()
seqs.pals[["Grays"]]       <-
  c(0,   NA,   0, NA, 10, 98, 1.3,  NA,  NA, 1) # ColorBrewer.org: Greys
seqs.pals[["Light Grays"]] <-
  c(0,   NA,   0, NA, 30, 90, 1.5,  NA,  NA, 1) # Z+KH+PM-09, Fig.5: Light Grays
seqs.pals[["Blues 2"]]     <-
  c(260,   NA,  80, NA, 30, 90, 1.5,  NA,  NA, 1) # Z+KH+PM-09, Fig.5: Blues
seqs.pals[["Blues 3"]]     <-
  c(245,   NA,  50, NA, 20, 98, 0.8, 1.4,  75, 1) # ColorBrewer.org: Blues
seqs.pals[["Purples 2"]]   <-
  c(270,   NA,  70, NA, 25, 95, 1.2,  NA,  NA, 1) # ColorBrewer.org: Purples
seqs.pals[["Purples 3"]]   <-
  c(270,   NA,  50, NA, 20, 98, 0.9, 1.4,  75, 1) # ColorBrewer.org: Purples
seqs.pals[["Reds 2"]]      <-
  c(10,   NA,  85, NA, 25, 95, 1.3,  NA,  NA, 1) # ColorBrewer.org: Reds
seqs.pals[["Reds 3"]]      <-
  c(10,   NA,  65, NA, 20, 97, 1.1, 1.3, 150, 1) # ColorBrewer.org: Reds
seqs.pals[["Greens 2"]]    <-
  c(135,   NA,  45, NA, 35, 95, 1.3,  NA,  NA, 1) # ColorBrewer.org: Greens
seqs.pals[["Greens 3"]]    <-
  c(135,   NA,  35, NA, 25, 98, 1.0, 1.5,  70, 1) # ColorBrewer.org: Greens
seqs.pals[["Oslo"]]        <-
  c(250,   NA,   0,  0, 99,  1, 1.0,  NA,  70, 1) # scico: oslo

seqm.pals <- list()
seqm.pals[["Purple-Blue"]] <-
  c(300,  200,  60,  0, 25, 95, 0.7, 1.3,  NA, 1) # ColorBrewer.org: BuPu
seqm.pals[["Red-Purple"]]  <-
  c(10,-80,  80,  5, 25, 95, 0.7, 1.3,  NA, 1) # ColorBrewer.org: PuRd
seqm.pals[["Red-Blue"]]    <-
  c(0,-100,  80, 40, 40, 75, 1.0, 1.0,  NA, 1) # Z+KH+PM-09, Fig.5: Red-Blue
seqm.pals[["Purple-Orange"]] <-
  c(-83,   20,  65, 18, 32, 90, 0.5, 1.0,  NA, 1) # CARTO: PurpOr
seqm.pals[["Purple-Yellow"]] <-
  c(320,   80,  60, 20, 30, 95, 0.7, 1.3,  65, 1) # RS+GM+MD+Z-15, similar to Fig.4: Precipitation
seqm.pals[["Blue-Yellow"]] <-
  c(265,   80,  60, 10, 25, 95, 0.7, 2.0,  NA, 1) # ColorBrewer.org: YlGnBu
seqm.pals[["Green-Yellow"]] <-
  c(140,   80,  50, 10, 40, 97, 0.7, 1.8,  NA, 1) # ColorBrewer.org: YlGn
seqm.pals[["Red-Yellow"]]  <-
  c(10,   85,  80, 10, 25, 95, 0.4, 1.3,  NA, 1) # ColorBrewer.org: YlOrRd
seqm.pals[["Heat"]]        <-
  c(0,   90,  80, 30, 30, 90, 0.2, 2.0,  NA, 1) # JCF/Z: alternative to heat_hcl
seqm.pals[["Heat 2"]]      <-
  c(0,   90, 100, 30, 50, 90, 0.2, 1.0,  NA, 1) # Z+KH+PM-09, Fig.5: heat_hcl
seqm.pals[["Terrain"]]     <-
  c(130,    0,  80,  0, 60, 95, 0.1, 1.0,  NA, 1) # Z+KH+PM-09, Fig.5: terrain_hcl
seqm.pals[["Terrain 2"]]   <-
  c(130,   30,  65,  0, 45, 90, 0.5, 1.5,  NA, 1) # JCF/Z: alternative to terrain_hcl

seqm.pals[["Viridis"]]     <-
  c(300,   75,  40, 95, 15, 90, 1.0, 1.1,  NA, 1) # viridis::viridis
seqm.pals[["Plasma"]]      <-
  c(-100,  100,  60, 100, 15, 95, 2.0, 0.9,  NA, 1) # viridis::plasma
seqm.pals[["Inferno"]]     <-
  c(-100,   85,   0, 65,  1, 98, 1.1, 0.9, 120, 1) # viridis::inferno
seqm.pals[["Rocket"]]      <- 
  c(-70,   60,   0, 10,  2, 97, 0.8, 0.8, 130, 1) # viridis::rocket
seqm.pals[["Mako"]]        <- 
  c(325,  130,   0, 18,  2, 95, 1.0, 1.0,  70, 1) # viridis::mako

seqm.pals[["Dark Mint"]]   <-
  c(240,  130,  30, 33, 25, 95, 1.0,  NA,  NA, 1) # CARTO: Dark Mint
seqm.pals[["Mint"]]        <-
  c(205,  140,  40, 12, 34, 94, 0.5, 1.0,  NA, 1) # CARTO: Mint
seqm.pals[["BluGrn"]]      <-
  c(215,  120,  25, 30, 31, 88, 0.7, 1.1,  45, 1) # CARTO: BluGrn
seqm.pals[["Teal"]]        <-
  c(240,  180,  35, 15, 35, 92, 0.6, 1.1,  40, 1) # CARTO: Teal
seqm.pals[["TealGrn"]]     <-
  c(220,  125,  44, 50, 49, 90, 0.8, 1.2,  60, 1) # CARTO: TealGrn
seqm.pals[["Emrld"]]       <-
  c(224,  105,  23, 55, 25, 92, 1.5, 1.0,  NA, 1) # CARTO: Emrld
seqm.pals[["BluYl"]]       <-
  c(250,   90,  40, 55, 33, 98, 0.5, 1.0,  NA, 1) # CARTO: BluYl
seqm.pals[["ag_GrnYl"]]    <-
  c(225,   87,  27, 86, 34, 92, 0.9,  NA,  NA, 1) # CARTO: ag_GrnYl
seqm.pals[["Peach"]]       <-
  c(15,   50, 128, 30, 55, 90, 1.1,  NA,  NA, 1) # CARTO: Peach
seqm.pals[["PinkYl"]]      <-
  c(-4,   80, 100, 47, 55, 96, 1.0,  NA,  NA, 1) # CARTO: PinkYl
seqm.pals[["Burg"]]        <-
  c(-10,   10,  40, 40, 25, 85, 1.2, 1.0,  75, 1) # CARTO: Burg
seqm.pals[["BurgYl"]]      <-
  c(-10,   55,  45, 30, 30, 90, 0.7, 1.0,  80, 1) # CARTO: BurgYl
seqm.pals[["RedOr"]]       <-
  c(-3,   53,  75, 42, 44, 86, 0.8, 1.0,  90, 1) # CARTO: RedOr
seqm.pals[["OrYel"]]       <-
  c(5,   72, 120, 49, 56, 87, 1.0,  NA, 125, 1) # CARTO: OrYel
seqm.pals[["Purp"]]        <-
  c(270,  300,  55, 20, 42, 92, 0.6, 1.0,  60, 1) # CARTO: Purp
seqm.pals[["PurpOr"]]      <-
  c(-83,   20,  55, 18, 32, 90, 0.6, 1.0,  65, 1) # CARTO: PurpOr
seqm.pals[["Sunset"]]      <-
  c(-80,   78,  60, 55, 40, 91, 0.8, 1.0,  75, 1) # CARTO: Sunset
seqm.pals[["Magenta"]]     <-
  c(312,  358,  50, 24, 27, 85, 0.6, 1.1,  65, 1) # CARTO: Magenta
seqm.pals[["SunsetDark"]]  <-
  c(-35,   50,  55, 60, 30, 90, 1.2, 1.0, 120, 1) # CARTO: SunsetDark
seqm.pals[["ag_Sunset"]]   <-
  c(-85,   70,  70, 45, 25, 85, 0.6, 1.0, 105, 1) # CARTO: ag_Sunset
seqm.pals[["BrwnYl"]]      <-
  c(-20,   70,  30, 20, 20, 90, 1.0, 1.1,  60, 1) # CARTO: BrwnYl

seqm.pals[["YlOrRd"]]      <-
  c(5,   85,  75, 40, 25, 99, 1.6, 1.3, 180, 1) # ColorBrewer.org: YlOrRd
seqm.pals[["YlOrBr"]]      <-
  c(20,   85,  50, 20, 25, 99, 1.3, 1.5, 150, 1) # ColorBrewer.org: YlOrBr
seqm.pals[["OrRd"]]        <-
  c(0,   60,  90, 10, 25, 97, 1.0, 1.5, 135, 1) # ColorBrewer.org: OrRd
seqm.pals[["Oranges"]]     <-
  c(20,   55,  70, 10, 30, 97, 1.2, 1.3, 150, 1) # ColorBrewer.org: Oranges
seqm.pals[["YlGn"]]        <-
  c(160,   85,  25, 20, 25, 99, 1.2, 1.6,  70, 1) # ColorBrewer.org: YlGn
seqm.pals[["YlGnBu"]]      <-
  c(270,   90,  40, 25, 15, 99, 2.0, 1.5,  90, 1) # ColorBrewer.org: YlGnBu
seqm.pals[["Reds"]]        <-
  c(0,   35,  65,  5, 20, 97, 1.1, 1.3, 150, 1) # ColorBrewer.org: Reds
seqm.pals[["RdPu"]]        <-
  c(-70,   40,  45,  5, 15, 97, 1.0, 1.3, 100, 1) # ColorBrewer.org: RdPu
seqm.pals[["PuRd"]]        <-
  c(20,-95,  60,  5, 20, 97, 1.6, 1.1, 140, 1) # ColorBrewer.org: PuRd
seqm.pals[["Purples"]]     <-
  c(275,  270,  55,  5, 20, 99, 1.3, 1.3,  70, 1) # ColorBrewer.org: Purples
seqm.pals[["PuBuGn"]]      <-
  c(160,  320,  25,  5, 25, 98, 1.4, 1.2,  70, 1) # ColorBrewer.org: PuBuGn
seqm.pals[["PuBu"]]        <-
  c(240,  260,  30,  5, 25, 98, 1.5, 1.2,  70, 1) # ColorBrewer.org: PuBu
seqm.pals[["Greens"]]      <-
  c(135,  115,  35,  5, 25, 98, 1.0, 1.5,  70, 1) # ColorBrewer.org: Greens
seqm.pals[["BuGn"]]        <-
  c(125,  200,  30,  5, 25, 98, 1.4, 1.6,  65, 1) # ColorBrewer.org: BuGn
seqm.pals[["GnBu"]]        <-
  c(265,   95,  55, 10, 25, 97, 1.3, 1.7,  80, 1) # ColorBrewer.org: GnBu
seqm.pals[["BuPu"]]        <-
  c(320,  200,  40,  5, 15, 98, 1.2, 1.3,  65, 1) # ColorBrewer.org: BuPu
seqm.pals[["Blues"]]       <-
  c(260,  220,  45,  5, 25, 98, 1.2, 1.3,  70, 1) # ColorBrewer.org: Blues
seqm.pals[["Lajolla"]]     <-
  c(90,-20,  40,  5, 99,  5, 0.7, 0.8, 100, 1) # scico: lajolla
seqm.pals[["Turku"]]       <-
  c(10,  120,  20,  0, 95,  1, 1.7, 0.8,  55, 1) # scico: turku
seqm.pals[["Hawaii"]]      <- 
  c(-30,  200,  70, 35, 30, 92, 0.3, 1.0,  75, 1) # scico: hawaii
seqm.pals[["Batlow"]]      <- 
  c(270,  -40,  35, 35, 12, 88, 0.6, 1.1,  75, 1) # scico: batlow

dive.pals <- list()
dive.pals[["Blue-Red"]]    <-
  c(260,    0,  80, NA, 30, 90, 1.5,  NA,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (high luminance contrast)
dive.pals[["Blue-Red 2"]]  <-
  c(260,    0, 100, NA, 50, 90, 1.0,  NA,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (medium luminance contrast)
dive.pals[["Blue-Red 3"]]  <-
  c(255,   12,  50, NA, 20, 97, 1.0, 1.3,  80, 1) # ColorBrewer.org: RdBu
dive.pals[["Red-Green"]]   <-
  c(340,  128,  60, NA, 30, 97, 0.8, 1.5,  80, 1) # ColorBrewer.org: PiYG
dive.pals[["Purple-Green"]] <-
  c(300,  128,  30, NA, 20, 95, 1.0, 1.4,  65, 1) # ColorBrewer.org: PRGn
dive.pals[["Purple-Brown"]] <-
  c(270,   40,  30, NA, 20, 98, 0.8, 1.2,  70, 1) # ColorBrewer.org: PuOr
dive.pals[["Green-Brown"]] <-
  c(180,   55,  40, NA, 25, 97, 0.8, 1.4,  65, 1) # ColorBrewer.org: BrBG
dive.pals[["Blue-Yellow 2"]] <-
  c(265,   80,  80, NA, 40, 95, 1.2,  NA,  NA, 1) # Z+COW
dive.pals[["Blue-Yellow 3"]] <-
  c(265,   80,  80, NA, 70, 95, 0.5, 2.0,  NA, 1) # Z+COW
dive.pals[["Green-Orange"]] <-
  c(130,   43, 100, NA, 70, 90, 1.0,  NA,  NA, 1) # Z+KH+PM-09, Fig.6: Green-Orange (low luminance contrast)
dive.pals[["Cyan-Magenta"]] <-
  c(180,  330,  59, NA, 75, 95, 1.5,  NA,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (low luminance contrast)
dive.pals[["Tropic"]]      <-
  c(195,  325,  70, NA, 55, 95, 1.0,  NA,  NA, 1) # CARTO: Tropic
dive.pals[["Broc"]]        <-
  c(240,   85,  30, NA, 15, 98, 0.9,  NA,  45, 1) # scico: broc
dive.pals[["Cork"]]        <-
  c(245,  125,  30, NA, 15, 95, 0.9, 1.1,  55, 1) # scico: cork
dive.pals[["Vik"]]         <-
  c(240,   55,  45, NA, 15, 95, 0.8, 1.1,  65, 1) # scico: vik
dive.pals[["Berlin"]]      <-
  c(240,   15,  60, NA, 75,  5, 1.2, 1.5,  80, 1) # scico: berlin
dive.pals[["Lisbon"]]      <-
  c(240,   85,  30, NA, 98,  8, 1.0,  NA,  45, 1) # scico: lisbon
dive.pals[["Tofino"]]      <-
  c(260,  120,  45, NA, 90,  5, 0.8, 1.0,  55, 1) # scico: tofino

base.pals <- list()
base.pals[["rainbow"]]        <-
  c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default RGB rainbow
base.pals[["heat.colors"]]    <-
  c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default heatmap
base.pals[["topo.colors"]]    <-
  c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default topo colors
base.pals[["terrain.colors"]] <-
  c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default terrain colors
base.pals[["cm.colors"]]      <-
  c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default cyan magenta colors
base.pals[["bpy"]]            <-
  c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Analogous to sp::bpy.colors

## collect all hcl palettes
make_hcl_pals <- function() {
  ## collect all palettes by group
  qpals <- as.data.frame(do.call("rbind", qual.pals))
  rownames(qpals) <- names(qual.pals)
  qpals$type <- "Qualitative"

  spals <- as.data.frame(do.call("rbind", seqs.pals))
  rownames(spals) <- names(seqs.pals)
  spals$type <- "Sequential (single-hue)"

  mpals <- as.data.frame(do.call("rbind", seqm.pals))
  rownames(mpals) <- names(seqm.pals)
  mpals$type <- "Sequential (multi-hue)"

  dpals <- as.data.frame(do.call("rbind", dive.pals))
  rownames(dpals) <- names(dive.pals)
  dpals$type <- "Diverging"

  ## combine and rearrange
  pals <- rbind(qpals, spals, mpals, dpals)
  names(pals) <- c(vars.pal, "type")
  pals$type <- factor(
    pals$type,
    levels = c(
      "Qualitative",
      "Sequential (single-hue)",
      "Sequential (multi-hue)",
      "Diverging"
    )
  )
  pals$fixup <- as.logical(pals$fixup)
  pals <- pals[, c("type", names(pals)[!names(pals) %in% "type"])]
  return(pals)
}

.colorspace_set_info(hcl_pals = make_hcl_pals())

add_hcl_pals <- function(palette, type, parameters) {
  pals <- .colorspace_get_info("hcl_pals")
  p <- data.frame(type = factor(type, levels = levels(pals$type)))
  p <- cbind(p, as.data.frame(as.list(parameters)))
  p$fixup <- as.logical(p$fixup)
  pals[palette,] <- p
  pals <- pals[order(pals$type),]
  .colorspace_set_info(hcl_pals = pals)
}

# -------------------------------------------------------------------
# Helper function: returns a data.frame containing all
# palettes specified above. Used for hclwizard and tcltk interface.
# @param gui, `NULL` or logical. If ``NULL` all palettes will be
# returned. If TRUE or FALSE the palettes will be subsetted and the
# data.frame will be slightly modified to fulfill the requirements
# for the graphical user interfaces (tcltk and shiny).
# -------------------------------------------------------------------
GetPaletteConfig <- function(gui = NULL) {
  res <- NULL
  palettes <- list(
    qual = qual.pals,
    seqs = seqs.pals,
    seqm = seqm.pals,
    dive = dive.pals,
    base = base.pals
  )
  res <- lapply(names(palettes), function(type, palettes) {
    # Palette parameters to data.frame
    x <- as.data.frame(do.call(rbind, palettes[[type]]))
    names(x) <- toupper(vars.pal)
    # Append type
    cbind(data.frame("type" = rep(type, nrow(x))), x)
  }, palettes = palettes)
  # Return data.frame
  pals      <- do.call(rbind, res)
  pals$type <- as.character(pals$type)

  if (inherits(gui, "logical")) {
    take <- c(
      # Qualitative
      "Pastel 1",
      "Dark 2",
      "Dark 3",
      "Set 2",
      "Set 3",
      "Warm",
      "Cold",
      "Harmonic",
      "Dynamic",
      # Diverging
      "Blue-Red",
      "Blue-Red 2",
      "Blue-Yellow 2",
      "Blue-Yellow 3",
      "Green-Orange",
      "Cyan-Magenta",
      "Tropic",
      # Diverging advanced
      "Blue-Red 3",
      "Red-Green",
      "Purple-Green",
      "Purple-Brown",
      "Green-Brown",
      "Cork",
      "Berlin",
      "Lisbon",
      "Tofino",
      # Sequential single-hue
      "Grays",
      "Light Grays",
      "Blues 2",
      "Purples 2",
      "Reds 2",
      "Greens 2",
      # Sequential single-hue advanced
      "Blues 3",
      "Purples 3",
      "Reds 3",
      "Greens 3",
      "Oslo",
      # Sequential multiple-hues
      "Purple-Blue",
      "Purple-Orange",
      "Red-Blue",
      "Red-Purple",
      "Red-Yellow",
      "Heat",
      "PinkYl",
      "Green-Yellow",
      "Terrain 2",
      "Dark Mint",
      "BluYl",
      "Blue-Yellow",
      "Viridis",
      "Plasma",
      # Sequential multiple-hues advanced
      "Purple-Yellow",
      "YlGnBu",
      "Greens",
      "BuGn",
      "Teal",
      "Peach",
      "Blues",
      "BuPu",
      "Purples",
      "Purp",
      "Burg",
      "Reds",
      "YlOrRd",
      "Sunset",
      "RdPu",
      "Inferno",
      "Lajolla",
      "Turku",
	  "Rocket",
	  "Mako",
	  "Hawaii",
	  "Batlow",
      # Base color maps (for shiny)
      "rainbow",
      "heat.colors",
      "topo.colors",
      "terrain.colors",
      "cm.colors",
      "bpy"
    ) # end of variable definition for "take"

    # For qualitative: set h2 if h2 is NA (else the sliders will
    # be disabled on the graphical user interfaces).
    idx <- which(pals$type == "qual" & is.na(pals$H2))
    pals$H2[idx] <-
      ifelse((pals$H1[idx] + 360) > 360, pals$H1[idx] - 360, pals$H1[idx] + 360)

    # Subset
    mtch <- match(take, rownames(pals))
    pals <- pals[mtch, ]
    #pals <- pals[which(rownames(pals) %in% take),]

    # Extending the type names for use in GUIs
    idx <- which(with(pals, (
      type == "dive" &  !is.na(CMAX) |
        (type == "seqs" &
           (!is.na(CMAX) | !is.na(P2))) |
        (type == "seqm" &
           (!is.na(CMAX) | C1 > 100 | C2 > 100))
    )))
    if (length(idx) > 0)
      pals$type[idx] <- sprintf("%s_advanced", pals$type[idx])
  }

  return(pals)
}

GetPalette <-
  function(...) {
    #type, h1, h2, c1, c2, l1, l2, p1, p2, fixup, reverse, cmax, register) {

    # Input arguments to list and make fixup logical
    arg <- list(...)
    arg$fixup <- as.logical(arg$fixup)

    # Qualitative color palettes
    if (grepl("^(qual|.*[Qq]ualitative)", arg$type)) {
      f <- colorspace::qualitative_hcl
      formals(f) <-
        eval(substitute(
          alist(
            n = ,
            h = hh,
            c = d1,
            l = d2,
            fixup = d3,
            gamma = NULL,
            alpha = 1,
            palette = NULL,
            rev = d4,
            register = d5,
            ... = ,
            h1 = ,
            h2 = ,
            c1 = ,
            l1 = ,
            cmax =
          ),
          list(
            hh = c(arg$h1, arg$h2),
            d1 = arg$c1,
            d2 = arg$l1,
            d3 = arg$fixup,
            d4 = arg$reverse,
            d5 = arg$register
          )
        ))

      # Sequential single-hue palettes
    } else if (grepl("^(seqs|.*[Ss]equential.*single)", arg$type)) {
      f <- colorspace::sequential_hcl
      formals(f) <-
        eval(substitute(
          alist(
            n = ,
            h = d1,
            c = d2,
            l = d3,
            power = d4,
            gamma = NULL,
            fixup = d5,
            alpha = 1,
            palette = NULL,
            rev = d6,
            register = d7,
            ... = ,
            h1 = ,
            h2 = ,
            c1 = ,
            c2 = ,
            l1 = ,
            l2 = ,
            p1 = ,
            p2 = ,
            cmax = ,
            c. =
          ),
          list(
            d1 = arg$h1,
            d2 = c(arg$c1, arg$cmax, arg$c2),
            d3 = c(arg$l1, arg$l2),
            d4 = arg$p1,
            d5 = arg$fixup,
            d6 = arg$reverse,
            d7 = arg$register
          )
        ))

      # Sequential multi-hue palettes
    } else if (grepl("^(seqm|.*[Ss]equential.*multi)", arg$type)) {
      f <- colorspace::sequential_hcl
      formals(f) <-
        eval(substitute(
          alist(
            n = ,
            h = d1,
            c = d2,
            l = d3,
            power = d4,
            gamma = NULL,
            fixup = d5,
            alpha = 1,
            palette = NULL,
            rev = d6,
            register = d7,
            ... = ,
            h1 = ,
            h2 = ,
            c1 = ,
            c2 = ,
            l1 = ,
            l2 = ,
            p1 = ,
            p2 = ,
            cmax = ,
            c. =
          ),
          list(
            d1 = c(arg$h1, arg$h2),
            d2 = c(arg$c1, arg$cmax, arg$c2),
            d3 = c(arg$l1, arg$l2),
            d4 = c(arg$p1, arg$p2),
            d5 = arg$fixup,
            d6 = arg$reverse,
            d7 = arg$register
          )
        ))

      # Diverging color palettes
    } else if (grepl("^(dive|.*[Dd]iverging)", arg$type)) {
      f <- colorspace::diverging_hcl
      arg_names <- names(arg[!is.na(arg)])
      if (all(c("p1", "p2")   %in% arg_names))
        power  <- c(arg$p1, arg$p2)
      else
        power  <- arg$p1
      if (all(c("c1", "cmax") %in% arg_names))
        chroma <- c(arg$c1, arg$cmax)
      else
        chroma <- arg$c1
      formals(f) <-
        eval(substitute(
          alist(
            n = ,
            h = d1,
            c = d2,
            l = d3,
            power = d4,
            gamma = NULL,
            fixup = d5,
            alpha = 1,
            palette = NULL,
            rev = d6,
            register = d7,
            ... = ,
            h1 = ,
            h2 = ,
            c1 = ,
            l1 = ,
            l2 = ,
            p1 = ,
            p2 = ,
            cmax = d8
          ),
          list(
            d1 = c(arg$h1, arg$h2),
            d2 = chroma,
            d3 = c(arg$l1, arg$l2),
            d4 = power,
            d5 = arg$fixup,
            d6 = arg$reverse,
            d7 = arg$register,
            d8 = arg$cmas
          )
        ))
    }
    f
  }
