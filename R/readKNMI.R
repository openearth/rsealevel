
#' get_knmi_daydata
#'
#' @param start start date (yyyymmdd) 
#' @param end end date (yyyymmdd) 
#' @param vars variables (see: https://www.knmi.nl/kennis-en-datacentrum/achtergrond/data-ophalen-vanuit-een-script) 
#' @param stns stations (see: https://www.knmi.nl/kennis-en-datacentrum/achtergrond/data-ophalen-vanuit-een-script)
#' @param prefer_json toggles csv or json output 
#'
#' @returns KNMI daily average values in memory
#' @export
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @import stringr
#'
#' @examples
#' # example
#' knmi_daily <- get_knmi_daydata(start = "19950101", end = "20250101", vars = c("FG:FHX:FHXH"), prefer_json = FALSE)
#' 
get_knmi_daydata <- function(start, end, vars = "Q:TG", stns = "310", prefer_json = TRUE) {
  
  url <- "https://www.daggegevens.knmi.nl/klimatologie/daggegevens"
  
  # JSON preferred
  if (prefer_json) {
    body <- list(start = start, end = end, vars = vars, stns = stns, fmt = "json")
    resp <- POST(url, body = body, encode = "form")
    if (!http_error(resp)) {
      j <- content(resp, "text", encoding = "UTF-8")
      x <- fromJSON(j)
      df <- as.data.frame(x$data, stringsAsFactors = FALSE)
      names(df) <- x$header
      
      out <- df %>%
        mutate(datum = as.Date(as.character(YYYYMMDD), format = "%Y%m%d")) %>%
        pivot_longer(cols = -c(STN, YYYYMMDD, datum),
                     names_to = "parameter",
                     values_to = "value") %>%
        mutate(value = as.numeric(value)) %>%
        select(datum, STN, parameter, value) %>%
        arrange(datum, STN, parameter)
      
      return(out)
    }
  }
  
  # CSV fallback
  body <- list(start = start, end = end, vars = vars, stns = stns, fmt = "csv")
  resp <- POST(url, body = body, encode = "form")
  stop_for_status(resp)
  
  txt <- content(resp, "text", encoding = "UTF-8")
  lines <- readLines(textConnection(txt), warn = FALSE)
  
  # Extract units from header lines
  var_lines <- grep("^#\\s*[A-Z0-9]+\\s*:", lines, value = TRUE)
  unit_map <- tibble(
    parameter = gsub("^#\\s*([A-Z0-9]+).*", "\\1", var_lines),
    unit = str_extract(var_lines, "\\(.*?\\)") %>% str_replace_all("[()]", "")
  )
  
  # Find header and data lines
  data_hdr_i <- which(grepl("^#\\s*STN\\s*,\\s*YYYYMMDD", lines))
  header <- strsplit(sub("^#\\s*", "", lines[data_hdr_i[1]]), ",")[[1]] |> trimws()
  data_lines <- lines[(data_hdr_i[1] + 1):length(lines)]
  data_lines <- data_lines[!grepl("^#", data_lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]
  
  clean_csv <- paste(data_lines, collapse = "\n")
  df <- read.csv(text = clean_csv, sep = ",", header = FALSE,
                 col.names = header, fill = TRUE, stringsAsFactors = FALSE)
  
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]
  
  out <- df %>%
    mutate(datum = as.Date(as.character(YYYYMMDD), format = "%Y%m%d")) %>%
    pivot_longer(cols = -c(STN, YYYYMMDD, datum),
                 names_to = "parameter",
                 values_to = "value") %>%
    mutate(value = as.numeric(value)) %>%
    left_join(unit_map, by = "parameter") %>%
    select(datum, STN, parameter, value, unit) %>%
    arrange(datum, STN, parameter)
  
  return(out)
}





#' read_knmi_hourly
#' Reads files as downloaded via https://www.knmi.nl/nederland-nu/klimatologie/uurgegevens

#' @param filename file name of downloaded file
#' @param n_max number of lines to read 
#'
#' @returns KNMI hourly average values in memory
#' @export
#' @import readr
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' # example
#' f <- system.file("extdata", "dekooy_uurgeg_wind_test.txt", package = "rsealevel")
#' read_knmi_hourly(f)

read_knmi_hourly <- function(filename, n_max = Inf) {
  
  lines <- readLines(filename, warn = FALSE)
  
  # Zoek de kolomheader
  # header_row <- grep("^#\\s*STN,YYYYMMDD", lines)
  header_row <- grep("^\\s*#?\\s*STN\\s*,\\s*YYYYMMDD", lines)
  
  if (length(header_row) == 0) {
    stop("Kolomheader niet gevonden.")
  }
  
  header <- strsplit(
    sub("^#\\s*", "", lines[header_row]),
    ","
  )[[1]]
  
  header <- trimws(header)
  
  data <- readr::read_csv(
    filename,
    skip = header_row,
    col_names = header,
    na = c("", " "),
    trim_ws = TRUE,
    n_max = n_max,
    show_col_types = FALSE
  )
  
  data |>
    dplyr::mutate(
      datetime = lubridate::ymd_h(
        sprintf("%08d %02d", YYYYMMDD, HH),
        tz = "UTC"
      )
    )
}


# filename = "data/knmi/metingen/uurgeg_235_2021-2030.txt"
# read_knmi_hourly(filename, n_max = Inf)

#' read_knmi_hourly_long
#' Reads files as downloaded via https://www.knmi.nl/nederland-nu/klimatologie/uurgegevens and transfers to long format

#' @param filename file name of downloaded file
#' @param n_max number of lines to read 
#'
#' @returns KNMI hourly average values in memory
#' @export
#' @import readr
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' # example
#' f <- system.file("extdata", "dekooy_uurgeg_wind_test.txt", package = "rsealevel")
#' read_knmi_hourly_long(f)
read_knmi_hourly_long <- function(filename, n_max = Inf) {
  
  dat <- read_knmi_hourly(filename, n_max)
  
  vars <- setdiff(
    names(dat),
    c("STN", "YYYYMMDD", "HH", "datetime")
  )
  
  tidyr::pivot_longer(
    dat,
    cols = dplyr::all_of(vars),
    names_to = "parameter",
    values_to = "value"
  )
}

# read_knmi_hourly_long("datadekooy_uurgeg_wind_test.txt")
