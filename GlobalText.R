
Explain1010 <- function(input,value){
  observeEvent(input[[value]], {
    showModal(modalDialog(
      title = "SRT module explained",
      "This tab provides guidance for issuer regulatory capital treatment of Significant Risk Transfer transactions.",tags$p(""),
      
      # Below creates bullet list --->
      tags$ul(
        tags$li(p(style="text-align: justify;",
                  "PENDING:")),
        tags$ul(
          tags$li(p(style="text-align: justify;",
                    "PENDING")),
          tags$li(p(style="text-align: justify;",
                    "PENDING")),
          tags$li(p(style="text-align: justify;",
                    "PENDING")),
          tags$li(p(style="text-align: justify;",
                    "PENDING"))
        ), # close tags$ul
        tags$li(p(style="text-align: justify;",
                  "PENDING")),
        tags$li(p(style="text-align: justify;",
                  "PENDING")),
        tags$li(p(style="text-align: justify;",
                  "PENDING")),
        tags$li(p(style="text-align: justify;",
                  "PENDING")),
        tags$li(p(style="text-align: justify;",
                  "PENDING"))
      ) # close tags$ul
    )) # close show Modal
  })   # close observe Event
} # close function

