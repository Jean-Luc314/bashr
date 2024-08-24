new_member_data <- function(
    .data = tibble::tibble(
      identifier = character(),
      payment_status = character(),
      member_status = character(),
      gender = character(),
      dob = as.Date(character()),
      date_benefit_commenced = as.Date(character()), 
      date_left_scheme = as.Date(character()),
      date_of_retirement = as.Date(character()),
      date_of_exit = as.Date(character()),
      pension_total_l1 = numeric(),
      pension_total_l2 = numeric()
    )
) {
  stopifnot(tibble::is_tibble(.data))
  class(.data) <- c("member_data", class(.data))
  .data
}

all_is_date <- function(dte) all(lubridate::is.Date(dte))
validate_pension <- function(x) all(x >= 0 | is.na(x))


#' Validate data
#' 
#' `validate_data()` accepts a data frame and performs two operations:
#' 
#' 1. Check fields satisfy data requirements specified in `data_spec$validators`.
#' 2. Possibly modify fields using `data_spec$transformers`.
#' 
#' `data_spec$validators` and `data_spec$transformers` will be named lists of functions, where the names correspond to fields in `.data`.
#' 
#' `dplyr::summarise()` is called on each field in `data_spec$validators`. `validate_data()` will fail if any field returned `FALSE`.
#' 
#' `dplyr::mutate()` is called on each field in `data_spec$transformers`.
#' 
#' @param .data A tibble contains at least fields from `names(data_spec$validators)` and `names(data_spec$transformers)`.
#' @param data_spec A list of `validators` and `transformers` that will operate on fields in `.data`.
#' 
#' `validators` and `transformers` will be a named list of functions, where the names correspond to fields within `.data`.
#' E.g., data_spec <- list(validators = list(gender = \(x) all(x %in% c("M", "F"))), transformers = list(gender = as.character))
#'
#' @return A tibble, possibly modified using `data_spec$transformers`.
#' @export
#'
#' @examples
#' .data <- tibble::tibble(identifier = c(1, 2), gender = "m")
#' check_unique <- \(x) !any(duplicated(x))
#' check_gender <- \(x) all(x %in% c("M", "F"))
#' check_dob <- as.Date
#' 
#' # Pass
#' validate_data(.data, list(validators = list(identifier = check_unique)))
#' 
#' # Missing dob field
#' validate_data(.data, list(validators = list(identifier = check_unique, gender = check_gender, dob = check_dob)))
#' 
#' # Fail gender value test
#' validate_data(.data, list(validators = list(identifier = check_unique, gender = check_gender)))
#' 
#' # Convert identifier to character
#' validate_data(.data, list(validators = list(identifier = check_unique), transformers = list(identifier = as.character)))
validate_data <- function(.data, data_spec) {
  .data_name <- deparse(substitute(.data))
  validators <- data_spec$validators
  validation_fields <- names(validators)
  transformers <- data_spec$transformers
  transform_fields <- names(transformers)
  
  # Check Missing Fields
  check_missing <- \(fields) {
    missing_fields <- fields[! fields %in% names(.data)]
    if (length(missing_fields) > 0) stop(.data_name, " is missing fields: ", paste(missing_fields, collapse = ", "), call. = FALSE)
  }
  check_missing(validation_fields)
  check_missing(transform_fields)
  
  # Check Validations Pass
  failed_validations <- .data |>
    dplyr::summarise(dplyr::across(dplyr::all_of(validation_fields),
                                   \(x) validators[[dplyr::cur_column()]](x))) |>
    dplyr::select(tidyselect::where(`!`)) |>
    names()
  if (length(failed_validations) > 0) stop(.data_name, " failed validations: ", paste(failed_validations, collapse = ", "), call. = FALSE)
  
  .data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(transform_fields),
                                \(x) transformers[[dplyr::cur_column()]](x)))
}

expand_data_spec <- function(data_spec, validators = list(), transformers = list()) {
  data_spec$validators <- append(data_spec$validators, validators)
  data_spec$transformers <- append(data_spec$transformers, transformers)
  data_spec
}

data_spec <- list(
  validators = list(
    identifier = \(x) { length(x) == length(unique(x)) },
    payment_status = \(x) { all(x %in% c("IN PMT", "DEATH", "EXIT")) },
    member_status = \(x) { all(x %in% c("DEF", "PEN", "SPS")) },
    gender = \(x) { all(x %in% c("M", "F")) },
    dob = all_is_date,
    date_benefit_commenced = all_is_date,
    date_left_scheme = all_is_date,
    date_of_retirement = all_is_date,
    date_of_exit = all_is_date,
    pension_total_l1 = validate_pension,
    pension_total_l2 = validate_pension
  ),
  transformers = list(
    identifier = as.character,
    payment_status = \(x) { factor(x, levels = c("IN PMT", "DEATH", "EXIT")) },
    member_status = \(x) { factor(x, levels = c("DEF", "PEN", "SPS")) },
    gender = \(x) { factor(x, levels = c("M", "F")) },
    dob = as.Date,
    date_benefit_commenced = as.Date,
    date_left_scheme = as.Date,
    date_of_retirement = as.Date,
    date_of_exit = as.Date,
    pension_total_l1 = as.numeric,
    pension_total_l2 = as.numeric
  )
)

expand_data_spec(data_spec,
                 validators = list(pension_tranche_t1_l1 = is.numeric),
                 transformers = list(pension_tranche_t1_l1 = as.numeric))

member_data <- new_member_data()
validate_data(member_data, data_spec)

