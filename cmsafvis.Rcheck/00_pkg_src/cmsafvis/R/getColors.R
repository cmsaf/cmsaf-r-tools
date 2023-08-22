# Function for getting colors. Either the basic color schemes or from colorspace.
getColors <- function(PAL,
                      palettes,
                      num_brk,
                      reverse) {
  idx <- which(rownames(palettes) == PAL)
  name   <- PAL
  if (PAL == "tim.colors") {
    name <- "fields::tim.colors"
  }
  if (PAL == "bpy") {
    name <- "sp::bpy.colors"
  }
  curPAL <- as.list(palettes[idx, ])
  if (length(idx) == 0) {
    idx <- which(rownames(palettes) == "larry")
    name   <- "larry"
    curPAL <- as.list(palettes[idx, ])
    
    idx <- which(rownames(palettes) == "albedo")
    name   <- "albedo"
    curPAL <- as.list(palettes[idx, ])
    
    idx <- which(rownames(palettes) == "albedo2")
    name   <- "albedo2"
    curPAL <- as.list(palettes[idx, ])
    
    idx <- which(rownames(palettes) == "sunny")
    name   <- "sunny"
    curPAL <- as.list(palettes[idx, ])
  }

  if (curPAL$type == "base") {
    pal <- eval(parse(text = tolower(name)))
  } else if (curPAL$type == "more") {
    larry <- grDevices::colorRampPalette(c("#023858",
                                           "#0570B0",
                                           "#6EAAC8",
                                           "#53BD9F",
                                           "#99F0B2",
                                           "#CDFFCD",
                                           "#FFFFFF",
                                           "#FFF5BA",
                                           "#F5E09E",
                                           "#F5CD84",
                                           "#E1A564",
                                           "#CD853F",
                                           "#B66A28"))
    albedo <- grDevices::colorRampPalette(c("#000229", "#021446", "#010a35", "#092203",
                                            "#1c4b00", "#2f6700", "#508901",
                                            "#73a203", "#94ad03", "#948e04",
                                            "#8a7c02", "#837002", "#7c5e02",
                                            "#7d5e02", "#8a6a10", "#997c21",
                                            "#a3882e", "#ae963b", "#baa448",
                                            "#c9b257", "#ccb55a", "#dec96a",
                                            "#e3ce6f", "#e6d272", "#efdd7d",
                                            "#f3e797", "#f3e798", "#f4eec0",
                                            "#f8f8f5", "#fefefe", "#fefefe", "#fefefe"))
    
    albedo2 <- grDevices::colorRampPalette(c(
      "#cbcbcc", "#0100e5", "#0100e5", "#0100e5", "#0100e5", "#0a5d02", "#096c01",
      "#098100", "#0b7e07", "#0c8f0a", "#0aa30b", "#08b40d", "#09c70d", "#30a618",
      "#568721", "#806431", "#836d2c", "#8a7930", "#928427", "#9b8d21", "#a2932b",
      "#a49625", "#ad9e1d", "#b6ab1d", "#bcb414", "#c4bd15", "#ccc517", "#d6ce13",
      "#e1d909", "#e0d810", "#e6e10b", "#f0ec06", "#f5f500", "#fefe03", "#ffe00b",
      "#ffcc06", "#ffb703", "#ffa102", "#ff8900", "#fe7208", "#ff6a00", "#f84804",
      "#ff2b03", "#fe1603", "#fc0202", "#fa0100", "#f70004", "#ec0202", "#e80001",
      "#e00001", "#e40002", "#df0003", "#dc0105", "#cf0105", "#cc0104", "#cd0100",
      "#c00105", "#be0000", "#b40103", "#b70100", "#aa0101", "#ac0000", "#a30001",
      "#980203", "#960104", "#930000", "#8f0002", "#900000", "#890000", "#650a0e"))
    
    # somehow sunny IS used
    sunny <- grDevices::colorRampPalette(c("black",
                                           "#3a0303",
                                           "#640000",
                                           "#981800",
                                           "#ca4b00",
                                           "#fc7f01",
                                           "#ffb234",
                                           "#ffe566",
                                           "#ffff98",
                                           "#ffffcb",
                                           "white"))
    cloud_mask1 <- grDevices::colorRampPalette(c("black", "transparent", "gray60", "white"))
    cloud_mask2 <- grDevices::colorRampPalette(c("black", "transparent", "gray60", "white", "pink"))
    pal <- eval(parse(text = tolower(name)))
  } else {
    curPAL$reverse <- FALSE
    pal <- do.call(GetPalette, curPAL)
  }

  colorbar <- pal(num_brk)

  if (reverse) {
    colorbar <- rev(colorbar)
  }

  return(colorbar)
}
