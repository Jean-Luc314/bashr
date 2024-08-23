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

is_date_all <- function(dte) all(lubridate::is.Date(dte))
validate_pension <- function(x) all(x >= 0 | is.na(x))

validate_member_data <- function(member_data,
                                 data_spec = list(
                                   identifier = \(x) { length(x) == length(unique(x)) },
                                   payment_status = \(x) { all(x %in% c("IN PMT", "DEATH", "EXIT")) },
                                   member_status = \(x) { all(x %in% c("DEF", "PEN", "SPS")) },
                                   gender = \(x) { all(x %in% c("M", "F")) },
                                   dob = is_date_all,
                                   date_benefit_commenced = is_date_all,
                                   date_left_scheme = is_date_all,
                                   date_of_retirement = is_date_all,
                                   date_of_exit = is_date_all,
                                   pension_total_l1 = validate_pension,
                                   pension_total_l2 = validate_pension
                                 )) {
  # Check Missing Fields
  missing_fields <- names(data_spec)[! names(data_spec) %in% names(member_data)]
  if (length(missing_fields) > 0) stop("member_data is missing fields: ", paste(missing_fields, collapse = ", "), call. = FALSE)
  
  # Check Validations Pass
  pass_validations <- member_data |> dplyr::summarise(dplyr::across(names(data_spec), \(x) data_spec[[dplyr::cur_column()]](x)))
  failed_validations <- pass_validations |> dplyr::select(tidyselect::where(`!`)) |> names()
  if (!all(pass_validations)) stop("member_data failed validations: ", paste(failed_validations, collapse = ", "), call. = FALSE)
  
  member_data
}


member_data <- new_member_data()
validate_member_data(member_data = new_member_data(df = tibble::tibble(identifier = c(1, 1))),
                     data_spec = list(identifier = \(x) length(x) == length(unique(x))))
validate_member_data(member_data)
