library(shiny)

mod_about_ui_sidebar <- function(id) {
  # NOTE: uses parent tab id "tabselected" -> don't namespace it
  conditionalPanel(
    condition = "input.tabselected=='about_model'",
    fluidRow(
      helpText(
        h5(strong("Base Input Panel")),
        align = "center",
        style = "margin-top:-15px; margin-bottom:5px"
      )
    ),
    h4("Select model type to run:  Select asset type:"),
    style = "display: none;"  # prevent flash on load
  )
}

mod_about_ui_main <- function(id) {
  tagList(
    helpText("Dynamic model for revolving asset pool"),
    h5(HTML(paste("<b>Developer:</b> this is a dummy module to illustrate",
                  "how modules plug into a Master Parent App")))
    
  )
}

mod_about_tab <- function(id) {
  tabPanel(
    "About model", value = "about_model",
    mod_about_ui_main(id)
  )
}

mod_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic today; placeholder to keep the pattern consistent.
    # Return a list later if you decide to expose reactives.
    invisible(NULL)
  })
}

# Omitted dummy parent app in this very simple module example