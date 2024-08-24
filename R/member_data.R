new_member_data <- function(
    df = tibble::tibble(
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
  class(df) <- c("member_data", class(df))
  df
}

all_is_date <- function(dte) all(lubridate::is.Date(dte))
validate_pension <- function(x) all(x >= 0 | is.na(x))

validate_member_data <- function(member_data,
                                 data_spec = list(
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
                                 )) {
  validators <- data_spec$validators
  validation_fields <- names(validators)
  transformers <- data_spec$transformers
  transform_fields <- names(transformers)
  
  # Check Missing Fields
  missing_fields <- validation_fields[! validation_fields %in% names(member_data)]
  if (length(missing_fields) > 0) stop("member_data is missing fields: ", paste(missing_fields, collapse = ", "), call. = FALSE)
  
  # Check Validations Pass
  failed_validations <- member_data |>
    dplyr::summarise(dplyr::across(dplyr::all_of(validation_fields),
                                   \(x) validators[[dplyr::cur_column()]](x))) |>
    dplyr::select(tidyselect::where(`!`)) |>
    names()
  if (length(failed_validations) > 0) stop("member_data failed validations: ", paste(failed_validations, collapse = ", "), call. = FALSE)
  
  member_data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(transform_fields),
                                \(x) transformers[[dplyr::cur_column()]](x)))
}


member_data <- new_member_data()
validate_member_data(member_data)
validate_member_data(member_data = new_member_data(df = tibble::tibble(identifier = c(1, 2))),
                     data_spec = list(
                       validators = list(
                         identifier = \(x) length(x) == length(unique(x))
                       ),
                       transformers = list(
                         identifier = as.character
                       )
                       )
                     )
