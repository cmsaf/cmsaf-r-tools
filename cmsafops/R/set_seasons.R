set_seasons <- function(yl, year, mon) {
  dummy_win <- NULL
  dummy_spr <- NULL
  dummy_sum <- NULL
  dummy_aut <- NULL

  for (i in seq_along(yl)) {
    win <- which(year == yl[i] & mon %in% c(1:2) | year == yl[i] - 1 & mon == 12)
    if (length(win) >= 3) {
      dummy_win <- append(dummy_win, win)
    }
    spr <- which(year == yl[i] & mon %in% c(3:5))
    if (length(spr) >= 3) {
      dummy_spr <- append(dummy_spr, spr)
    }
    sum <- which(year == yl[i] & mon %in% c(6:8))
    if (length(sum) >= 3) {
      dummy_sum <- append(dummy_sum, sum)
    }
    aut <- which(year == yl[i] & mon %in% c(9:11))
    if (length(aut) >= 3) {
      dummy_aut <- append(dummy_aut, aut)
    }
  }

  return(list(win = dummy_win, spr = dummy_spr, sum = dummy_sum, aut = dummy_aut))
}
