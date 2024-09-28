#' Build a Function with an package SHA condition
#'
#' `wrap_sha_condition` returns a function that checks a package's SHA is valid before execution.
#'
#' It is a wrapper that essentially returns `function(code) run_on_sha_match(package, expect_sha, code)`.
#'
#' It can be used to save repeated calls to `package` and `expect_sha`.
#'
#' @param package the name of a package, given as a name or character string.
#' @param expect_sha a character vector defining the SHA values that allow `code` to execute.
#'
#' @return a function that runs `code` to execute. Use braces ({}) for multi-line executions.
#' @export
#'
#' @examples
#' remotes::install_git("https://github.com/tidyverse/ggplot2.git", ref = "4fbc8575d80e54e1973ac58e979c2300aec21394")
#' safely_run_ggplot2 <- wrap_sha_condition(ggplot2, "4fbc8575d80e54e1973ac58e979c2300aec21394")
#' safely_run_ggplot2(({print("Exectuted"); 2 * 8}))
#'
wrap_sha_condition <- function(package, expect_sha) {
  purrr::partial(run_on_sha_match, rlang::enexpr(package), expect_sha)
}
