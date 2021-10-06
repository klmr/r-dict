box::use(stats[setNames])

#' Create a list with computed names
#'
#' \code{dict()} creates a list with names that can be arbitrary, computed
#' expressions.
#' @param ... expressions of the form \code{n : v}, where \code{n} and \code{v}
#'  form the name and the value, respectively, of each item. Both can be
#'  arbitrary expressions.
#'
#' If the package \pkg{glue} can be loaded, then \code{dict()} automatically
#' interpolates expressions in names using \code{\link[glue]{glue}}.
#' @example
#' # Simple list; same as list(a = 1, b = 2)
#' dict('a': 1, 'b': 2)
#'
#' x = 1 + 2
#' dict(paste0('a', x + 1): 1, paste('b', x + 2): 2)
#'
#' # With \pkg{glue} support:
#' dict('a{x + 1}': 1, 'b{x + 2}': 2)
#' @export
dict = function (...) {
    caller = parent.frame()
    dots = match.call(expand.dots = FALSE)$...
    lapply(dots, function (e) stopifnot(is.call(e) && e[[1L]] == quote(`:`)))

    names = vapply(dots, name_interpolate, character(1L), caller)
    values = lapply(dots, function (e) eval(e[[3L]], caller))
    setNames(values, names)
}

.on_load = function (ns) {
    ns$name_interpolate = if (requireNamespace('glue')) {
        box::use(glue[glue])

        function (expr, caller) {
            glue(eval(expr[[2L]], caller), .envir = caller)
        }
    } else {
        function (expr, caller) {
            eval(expr[[2L]], caller)
        }
    }
}
