bib_file <- "RJreferences.bib"

bib_raw <- readBin(
  bib_file,
  what = "raw",
  n = file.info(bib_file)$size
)

bib_text <- rawToChar(bib_raw)

valid_utf8 <- !is.na(
  iconv(
    bib_text,
    from = "UTF-8",
    to = "UTF-8",
    sub = NA
  )
)

valid_utf8

## if true 
bib <- readLines(
  "RJreferences.bib",
  warn = FALSE,
  encoding = "UTF-8"
)

hit <- grepl("[^\x01-\x7F]", bib)

cat(
  paste0(which(hit), ": ", bib[hit]),
  sep = "\n"
)


## fix
file.copy(
  "RJreferences.bib",
  "RJreferences_before_latex_accents.bib",
  overwrite = TRUE
)


bib <- readLines(
  "RJreferences.bib",
  warn = FALSE,
  encoding = "UTF-8"
)

replacements <- c(
  "Faragó" = "Farag{\\'o}",
  "István" = "Istv{\\'a}n",
  "Ágnes" = "{\\'A}gnes",
  "González-Parra" = "Gonz{\\'a}lez-Parra",
  "Nørsett" = "N{\\o}rsett",
  "Muñoz-Carpena" = "Mu{\\~n}oz-Carpena",
  "Runge–Kutta" = "Runge--Kutta",
  "Ecological–Societal" = "Ecological--Societal",
  "Richardson’s" = "Richardson's"
)

for (old in names(replacements)) {
  bib <- gsub(
    old,
    replacements[[old]],
    bib,
    fixed = TRUE
  )
}

writeLines(
  bib,
  "RJreferences.bib",
  useBytes = TRUE
)
