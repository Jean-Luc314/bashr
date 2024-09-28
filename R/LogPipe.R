wrap_with_log <- function(df, log = list()) {
  if (missing(log)) {
    log <- substitute(df)
  }
  list(
    df = df,
    log = log
  )
}

`%<$>%` <- function(lhs_logged, rhs) {
  lhs <- lhs_logged$df
  log <- lhs_logged$log
  lhs <- substitute(lhs)
  rhs <- substitute(rhs)
  new_log <- append(log, rhs)
  kind <- 1L
  env <- parent.frame()
  lazy <- TRUE
  a <- .External2(magrittr:::magrittr_pipe)
  
  wrap_with_log(a, new_log)
}

`%<$>%` <- function(lhs_logged, rhs) {
  lhs <- lhs_logged$df
  log <- lhs_logged$log
  new_log <- append(log, substitute(rhs))
  
  wrap_with_log(magrittr::`%>%`(lhs, rhs), new_log)
}

get_code <- function(df_with_log) {
  df_with_log$log %>%
    map_chr(deparse) %>%
    paste(collapse = " %>% ")
}

library(tidyverse)
library(palmerpenguins)

analysis <- wrap_with_log(penguins) %<$>%
  mutate(bill_area = bill_length_mm * bill_depth_mm) %<$>%
  group_by(species) %<$>%
  summarise(bill_area = mean(bill_area, na.rm = TRUE))

analysis %>% get_code()
