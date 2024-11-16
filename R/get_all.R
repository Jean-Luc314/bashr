#' Extract All Functions and Variables from a Namespace
#'
#' This function will enable the user to access all functions / variables within a Namespace.
#' E.g., the user could use it to access private functions from a package.
#'
#' By default, all items are imported into the parent environment.
#' E.g., values will be imported into the Global Environment if the user calls this function,
#'
#' @param package a symbol or string
#' @param envir an environment to import values into. E.g., `new.env()`. Defaults to `parent.frame()`.
#'
#' @return `envir` with items from `package` imported or overwritten.
#' @export
#'
#' @examples
#' get_all(dplyr); mutate.data.frame
#' dplyr <- get_all(dplyr, rlang::env()); dplyr$mutate.data.frame
get_all <- function(package, envir = parent.frame()) {

  package <- rlang::enexpr(package) # Convert to symbol. E.g., allow `package = ggplot2`

  pkg_envir <- getNamespace(package)

  purrr::walk(ls(pkg_envir), \(.f) assign(.f, get(.f, envir = pkg_envir), envir = envir))

  envir
}
