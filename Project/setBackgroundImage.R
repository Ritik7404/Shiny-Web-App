setBackgroundColor <- function(color = "ghostwhite",
                               gradient = c("linear", "radial"),
                               direction = c("bottom", "top", "right", "left"),
                               shinydashboard = FALSE) {
  gradient <- match.arg(gradient)
  direction <- match.arg(direction, several.ok = TRUE)
  if (length(color) == 1) {
    css <- sprintf("background-color: %s;", color)
  } else {
    if (length(direction) > 2) {
      direction <- direction[1]
    }
    direction <- paste(direction, collapse = " ")
    if (gradient == "radial") {
      direction <- paste("circle at", direction)
    } else {
      direction <- paste("to", direction)
    }
    css <- sprintf(
      "background: %s%s-gradient(%s, %s) fixed;",
      c("-moz-", "-webkit-", "-ms-", "-o-", ""),
      gradient, direction,
      paste(color, collapse = ", ")
    )
    css <- paste(css, collapse = "")
    if (!isTRUE(shinydashboard)) {
      css <- paste("position: absolute;", css)
    }
    css <- paste(
      "min-height: 100%; width:100%;
      -webkit-background-size: cover;
      -moz-background-size: cover;
      -o-background-size: cover;
      background-size: cover;",
      css
    )
  }
  if (isTRUE(shinydashboard)) {
    css <- paste0(".content-wrapper {", css, "}")
  } else {
    css <- paste0("body {", css, "}")
  }
  htmltools::tags$head(
    htmltools::tags$style(css)
  )
}


#' @title Custom background image for your shinyapp
#'
#' @description Allow to change the background image of your shinyapp.
#'
#' @param src Url or path to the image, if using local image,
#'  the file must be in \code{www/} directory and the path not contain \code{www/}.
#' @param shinydashboard Set to \code{TRUE} if in a {shinydasboard} application.
#'
#'
#' @export
#'
#' @importFrom htmltools tags
#'
#' @examples
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyWidgets)
#'
#' ui <- fluidPage(
#'   tags$h2("Add a shiny app background image"),
#'   setBackgroundImage(
#'     src = "https://www.fillmurray.com/1920/1080"
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'
#' }
#'
#' shinyApp(ui, server)
#'
#' }
#'
setBackgroundImage <- function(src = NULL, shinydashboard = FALSE) {
  if (isTRUE(shinydashboard)) {
    el <- ".content-wrapper"
  } else {
    el <- "body"
  }
  css <- paste0(
    el, " {background: url(", src, ") no-repeat center center fixed;
           -webkit-background-size: cover;
           -moz-background-size: cover;
           -o-background-size: cover;
           background-size: cover;}"
  )
  tags$head(tags$style(HTML(css)))
}