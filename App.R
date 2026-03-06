library(shiny)
library(shinyWidgets)

### Modules guide ###
# ModAbout dummy tab
# Mod90 for "SRT" tab
# Mod95 for temp tranche/class inputs to be fed in from other module in full code 
# Mod95 for temp portfolio and class amortisation and loss vectors to be fed in from other module in full code

source("./GlobalFunctions.R")
source("./GlobalText.R") # if needed globally
source(file.path("www", "mod90_support", "KeysSheet.R"), local = TRUE)
source(file.path("www", "mod90_support", "TableUIHelpers.R"), local = TRUE)
source(file.path("www", "mod90_support", "ERBA.R"), local = TRUE)
source(file.path("www", "mod90_support", "EconCore.R"), local = TRUE)
source(file.path("www", "mod90_support", "SEC-IRBA.R"), local = TRUE)
source(file.path("www", "mod90_support", "Output.R"), local = TRUE)

source("./ModAbout.R")       # defines mod_about_* functions
source("./Mod95.R")          # defines mod95_* functions
source("./Mod90.R")          # defines mod90_* functions (uses others)

# Below sets the threshold for invoking scientific notation
options(scipen=5)

options(shiny.dev.mode = TRUE)

ui <- 
  fluidPage(
    tags$head(
      # Load the specific mod90 CSS which now contains our professional fixes
      tags$link(rel = "stylesheet", type = "text/css", href = "mod90.css"),
      
      # Retain shell CSS if needed for other layouts
      tags$link(rel = "stylesheet", type = "text/css", href = "srt-shell.css"),
      
      tags$script(src = "jquery-ui.min.js"),
      tags$script(src = "mod90.js")
    ),
    
    sidebarLayout(
      # Increase width to 4 (default is 3) to make the Base Input Panel more professional
      sidebarPanel(
        width = 4,
        mod_about_ui_sidebar("about"),
        mod90_sidebar("mod90"),
        mod95_sidebar("mod95")
      ),
      
      mainPanel(
        width = 8, # Adjust main panel to complement the wider sidebar
        tabsetPanel(
          mod_about_tab("about"),
          mod90_tab("mod90"),
          mod95_tab("mod95"),
          id = "tabselected"
        ) 
      ) 
    ) 
  )

server <- function(input, output, session) {
  
  # Per-session SRT event bus (pub/sub). The Master stays a traffic-cop: create once and pass the SAME bus into all modules
  # Function bus_SRT() defined in GlobalFunctions.R
  bus <- bus_SRT()
  
  # --- Start sibling modules with the SAME bus ---
  # ID must match the UI IDs
  mod95_server("mod95",     bus = bus)   # publishes 'tranche_df', optional subscriber to 'mod90_summary'
  mod90 <- mod90_server("mod90", bus = bus)   # subscribes 'tranche_df', publishes 'mod90_summary'
  
  # --- If you still want top-level tab state in the parent, keep these: ---
  select_tab <- reactive({ input$tabselected })
  
  # IMPORTANT: parent can't read input$SRT_subtab directly (it's inside mod90)
  # Use the reactive returned by mod90_server(...)
  select_liab_subtab <- reactive({ req(mod90$subtab()); mod90$subtab() })
  
  tabsKey <- reactive({ paste0(select_tab(), "-", select_liab_subtab()) })
  
  # If other legacy code expects `common`, can keep it, or remove if fully migrated to bus
  common <- reactiveValues(
    select_tab = select_tab,
    select_liab_subtab = select_liab_subtab,
    tabsKey = tabsKey
  )
  
  onSessionEnded(function() { stopApp() })
}


shinyApp(ui, server)