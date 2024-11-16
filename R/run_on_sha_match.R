
#' Run code conditional on package SHA
#'
#' `run_on_sha_match()` lets you safely execute `code` when only valid `package` versions are installed.
#' It can be useful as a risk control to prevent unintended side-effects.
#' E.g., when `code` should only save files when specific package versions are installed.
#'
#' It checks `utils::packageDescription(package)$RemoteSha %in% expect_sha`.
#' When `TRUE`, `code` is executed. I.e., `eval(code, envir = parent.frame())`.
#' When `FALSE` an error is thrown.
#'
#' @param package the name of a package, given as a name or character string.
#' @param expect_sha a character vector defining the SHA values that allow `code` to execute.
#' @param code Code to execute. Use braces ({}) for multi-line executions.
#'
#' @return The result of `code`.
#' @export
#'
#' @examples
#' # Execute
#' remotes::install_git("https://github.com/tidyverse/ggplot2.git", ref = "4fbc8575d80e54e1973ac58e979c2300aec21394")
#' run_on_sha_match(ggplot2, "4fbc8575d80e54e1973ac58e979c2300aec21394", {
#'   print("Executed")
#'   2 * 2 == 4
#' })
#'
#' # Don't Execute
#' run_on_sha_match(ggplot2, "dummy", {
#'   print("Not Executed")
#'   2 * 2 == 4
#' })
run_on_sha_match <- function(package, expect_sha, code) {
  # Null Coalescing Operator exported from R v4.4.1+
  `%||%` <- function(x, y) if (is.null(x)) y else x
  wrap_quotes <- function(chr) paste0("\"", chr, "\"")

  package <- rlang::enexpr(package) # Convert to symbol. E.g., allow `package = ggplot2`

  pck_desc <- utils::packageDescription(package) # Get package meta data
  sha <- pck_desc$RemoteSha %||% "" # Extract SHA, coalesce to "" when NULL. E.g., installed from CRAN
  if (! sha %in% expect_sha) {
    stop(
      # Break if sha does not match any `expect_sha`
      "Incompatible version for package ", package,
      "\n",
      "Expected sha ", wrap_quotes(expect_sha),
      "\n",
      "Got sha ", wrap_quotes(sha)
    )
  }
  code <- substitute(code)
  eval(code, envir = parent.frame()) # Execute `code` in parent environment
}

