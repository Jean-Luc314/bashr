#' Links to ONS Price Inflation datasets
#'
#' @return named list of online addresses to the ONS site and source of price inflation data
#' @export
#'
#' @examples
#' get_price_inflation_source()
get_price_inflation_source <- function() {
  list(
    site = "https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceinflation",
    link = "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceinflation/current/consumerpriceinflationdetailedreferencetables.xlsx"
  )
}

#' Links to ONS Death datasets
#'
#' @param month string of month data is required. Months are lowercase in English. E.g., "january".
#' @param year integer of year data is required. E.g., 202.
#'
#' @return named list of online addresses to the ONS site and source of Death data
#' @export
#'
#' @examples
#' get_ons_deaths_source("january", 2024)
get_ons_deaths_source <- function(month, year) {
  stopifnot(month %in% c("january",
                         "february",
                         "march",
                         "april",
                         "may",
                         "june",
                         "july",
                         "august",
                         "september",
                         "october",
                         "november",
                         "december"))

  list(
    site = "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths",
    link = paste0(
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/monthlyfiguresondeathsregisteredbyareaofusualresidence/",
      year,
      "/monthlydeaths",
      month,
      year,
      ".xlsx"
    )
  )
}

#' Download, save, and read
#'
#' A wrapper around `download.file()`, with defaults using `[get_price_inflation_source]()`, to download and save price inflation data from the ONS.
#'
#' @param link a string of the link to download
#' @param file a string of the location to save the downloaded dataset
#'
#' @return An (invisible) integer code, 0 for success and non-zero for failure
#' @export
#'
#' @examples
#' download_price_inflation()
download_price_inflation <- function(link = get_price_inflation_source()$link, file = paste("data", basename(link), sep = "/")) {
  download.file(link, file, mode = "wb")
}

#' Extract inflation information, save tidy format
#'
#' A wrapper around `readxl::read_xlsx()` to read ONS Price Inflation into R.
#'
#' @param price_inflation_file a string of the price inflation file
#' @param sheet a string of the sheet to read
#' @param range a string of the range to read
#'
#' @return a tibble
#' @export
#'
#' @examples
#' # Download the dataset
#' # link <- get_price_inflation_source()$link
#' # file <- paste("data", basename(link), sep = "/")
#' # download_price_inflation(link, file)
#' # read_raw_price_inflation(file)
read_raw_price_inflation <- function(price_inflation_file, sheet = "Table 1", range = "B14:H51") {
  readxl::read_xlsx(price_inflation_file, sheet = sheet, range = range)
}

#' Clean ONS Price data
#'
#' 1. Set first column name to `month`
#' 2. Create a `date` field, set to 1st of month
#' 3. Rename `cpi = D7G7`, `rpi = CZBH`
#'
#' @param raw_price_inflation tibble of ONS price inflation data
#'
#' @return tibble with adjustments from the description
#' @export
#'
#' @examples
#' raw_price_inflation <- read_raw_price_inflation("data/consumerpriceinflationdetailedreferencetables.xlsx")
#' clean_price_inflation(raw_price_inflation)
clean_price_inflation <- function(raw_price_inflation) {

  inflation_tbl <- inflation_tbl_raw |>
    dplyr::rename(month = `...1`) |>
    dplyr::mutate(date = lubridate::`%m-%`(as.Date(paste("01", dplyr::last(month)), format = "%d %b %Y"),
                                           months(rev(dplyr::row_number() - 1)))) |>
    dplyr::select(month, date, cpi = D7G7, rpi = CZBH)
}

