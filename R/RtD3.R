#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
RtD3 <- function(message, data, width = NULL, height = NULL, elementId = NULL) {

  # forward options using x
  x = list(
    message = message,
    data = jsonlite::toJSON(data)
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'RtD3',
    x,
    width = width,
    height = height,
    package = 'RtD3',
    elementId = elementId
  )
}

#' Shiny bindings for RtD3
#'
#' Output and render functions for using RtD3 within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a RtD3
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name RtD3-shiny
#'
#' @export
RtD3Output <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'RtD3', width, height, package = 'RtD3')
}

#' @rdname RtD3-shiny
#' @export
renderRtD3 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, RtD3Output, env, quoted = TRUE)
}
