extract_first_stream <- function(path, outpath) {
  b <- readBin(path, "raw", file.info(path)[["size"]])
  starts <- grepRaw("stream\n", b, fixed = TRUE, all = TRUE)
  keep <- vapply(
    starts,
    function(p) p <= 3 || rawToChar(b[(p - 3):(p - 1)]) != "end",
    logical(1)
  )
  starts <- starts[keep]
  ends <- grepRaw("\nendstream", b, fixed = TRUE, all = TRUE)
  st <- starts[1] + nchar("stream\n")
  en <- ends[which(ends > st)[1]] - 1
  writeBin(memDecompress(b[st:en], type = "unknown"), outpath)
}

parse_model_paths <- function(stream_file) {
  txt <- paste(readLines(stream_file, warn = FALSE), collapse = " ")
  matches <- gregexpr("1\\.920561 w(.*?) S", txt, perl = TRUE)[[1]]
  blocks <- regmatches(txt, list(matches))[[1]]

  paths <- lapply(blocks, function(block) {
    coords <- gregexpr(
      "(-?[0-9]+\\.?[0-9]*)\\s+(-?[0-9]+\\.?[0-9]*)\\s+[ml]",
      block,
      perl = TRUE
    )
    pieces <- regmatches(block, coords)[[1]]
    if (!length(pieces)) {
      return(matrix(numeric(), ncol = 2))
    }
    do.call(rbind, lapply(pieces, function(piece) {
      as.numeric(regmatches(
        piece,
        gregexpr("-?[0-9]+\\.?[0-9]*", piece, perl = TRUE)
      )[[1]][1:2])
    }))
  })

  paths[vapply(paths, nrow, integer(1)) > 10]
}

summarise_dca <- function(stream_file, outcome) {
  paths <- parse_model_paths(stream_file)

  if (outcome == "Readmission") {
    panels <- data.frame(
      col = rep(1:4, each = 2),
      row = rep(c("A", "B"), times = 4)
    )
    horizons <- c(6, 12, 36, 60)
    px <- c(59.422, 184.566, 309.707, 434.848)
    xzero_offset <- 5.101
    xscale <- (166.504 - 64.523) / 0.5
    yzero <- c(A = 67.332, B = 193.641)
    yscale <- (110.027 - 67.332) / 0.5
    focus <- data.frame(
      horizon = horizons,
      lo = c(.03, .05, .10, .12),
      hi = c(.15, .24, .41, .50)
    )
  } else {
    panels <- data.frame(
      col = rep(1:3, each = 2),
      row = rep(c("A", "B"), times = 3)
    )
    horizons <- c(12, 36, 60)
    px <- c(65.824, 247.211, 428.602)
    xzero_offset <- 7.656
    xscale <- (226.598 - 73.48) / 0.5
    yzero <- c(A = 51.875, B = 195.184)
    yscale <- 158.082 - 51.875
    focus <- data.frame(
      horizon = horizons,
      lo = c(.01, .01, .02),
      hi = c(.03, .06, .10)
    )
  }

  out <- data.frame()
  for (i in seq_along(paths)) {
    p <- paths[[i]]
    col <- panels$col[i]
    row <- as.character(panels$row[i])
    horizon <- horizons[col]

    x0 <- px[col] + xzero_offset
    threshold <- (p[, 1] - x0) / xscale
    nb <- (yzero[row] - p[, 2]) / yscale
    curve <- data.frame(threshold = threshold, nb = nb)

    f <- focus[focus$horizon == horizon, ]
    in_focus <- curve$threshold >= f$lo - 1e-4 & curve$threshold <= f$hi + 1e-4
    value_at <- function(th) approx(curve$threshold, curve$nb, xout = th, rule = 2)$y

    out <- rbind(out, data.frame(
      outcome = outcome,
      panel = row,
      horizon_months = horizon,
      focus_window = sprintf("%d-%d%%", round(100 * f$lo), round(100 * f$hi)),
      nb_start = value_at(f$lo),
      nb_mid = value_at((f$lo + f$hi) / 2),
      nb_end = value_at(f$hi),
      nb_min_focus = min(curve$nb[in_focus]),
      nb_max_focus = max(curve$nb[in_focus]),
      nb_mean_focus = mean(curve$nb[in_focus])
    ))
  }

  out
}

readm_stream <- "_tmp_docx_media/p_dca_readm_stream.txt"
death_stream <- "_tmp_docx_media/p_dca_death_stream.txt"

if (!file.exists(readm_stream)) {
  extract_first_stream(
    "C:/Users/homes/OneDrive/Escritorio/p_dca_readm.pdf",
    readm_stream
  )
}
if (!file.exists(death_stream)) {
  extract_first_stream(
    "C:/Users/homes/OneDrive/Escritorio/p_dca_death.pdf",
    death_stream
  )
}

estimates <- rbind(
  summarise_dca(readm_stream, "Readmission"),
  summarise_dca(death_stream, "Mortality")
)

print(estimates, digits = 4)
write.csv(estimates, "_tmp_docx_media/dca_estimates.csv", row.names = FALSE)
