#library(openxlsx)
#library(rhandsontable)

library(openxlsx)
library(rhandsontable)

source("./GlobalText.R")
source("./GlobalFunctions.R")

mod95_ui_input <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      helpText(
        h5(strong("Base Input Panel")),
        align = "center",
        style = "margin-top:-15px; margin-bottom:5px"
      )
    ),
    h4("Scenario"),
    checkboxInput(ns("sel_base"),       "Base",              value = TRUE),
    checkboxInput(ns("sel_stress"),     "Stress",            value = FALSE),
    checkboxInput(ns("sel_backloaded"), "Backloaded stress", value = FALSE),
    
    tags$hr(style = "margin:10px 0;"),
    checkboxInput(ns("sel_upload"), "Upload scenario", value = FALSE),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("sel_upload")),
      fileInput(
        ns("scenario_file"),
        label = "Upload scenario Excel (.xlsx / .xlsm)",
        accept = c(".xlsx", ".xlsm")
      ),
      helpText("Expected: a file with sheet named 'Sheet1' (fallback: first sheet).")
    )
  )
}

mod95_sidebar <- function(id, parent_tab_value = "mod_95") {
  conditionalPanel(
    condition = sprintf("input.tabselected == '%s'", parent_tab_value),
    mod95_ui_input(id),
    style = "display: none;"
  )
}

mod95_ui_output <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Tranches transmitted to mod90"),
    actionButton(ns("edit_tranches"), "Edit tranches", class = "btn btn-primary"),
    tags$div(style = "height:8px;"),
    rHandsontableOutput(ns("tranche_tbl")),
    tags$details(
      style = "border-top:1px solid #e5e7eb; margin-top:12px; padding-top:8px;",
      tags$summary("🔧 Debug: received from mod90"),
      tags$p("Most recent 'mod90_summary' bus payload:"),
      verbatimTextOutput(ns("dbg_mod90_summary"))
    ),
    tags$details(
      style = "border-top:1px solid #e5e7eb; margin-top:12px; padding-top:8px;",
      tags$summary("📄 Selected scenario preview (Sheet1)"),
      tableOutput(ns("scenario_preview"))
    )
  )
}

mod95_tab <- function(id) {
  tabPanel(
    "Tranches (mod95)", value = "mod_95",
    div(style = "margin-top: 5px"),
    mod95_ui_output(id)
  )
}

# --------------------------- helpers ---------------------------------

.normalise_names <- function(x) {
  y <- iconv(x, to = "UTF-8", sub = " ")
  y <- gsub("[^[:alnum:]]+", " ", y, perl = TRUE)
  y <- trimws(y)
  y <- gsub("\\s+", " ", y)
  tolower(y)
}

first_row_notionals <- function(df) {
  nms <- .normalise_names(names(df))
  r <- 1L
  per_idx <- which(nms == "period")
  if (length(per_idx) == 1L) {
    per_col <- df[[per_idx]]
    which1 <- which(as.numeric(per_col) == 1)
    if (length(which1)) r <- which1[1]
  }
  
  pick <- function(letter) {
    j_all <- which(grepl(paste0("^class\\s*", letter, "\\s*balance$"), nms))
    if (!length(j_all)) return(NA_real_)
    j <- j_all[1L]
    raw <- as.character(df[[j]][r])
    if (is.null(raw) || identical(raw, "") || is.na(raw)) return(NA_real_)
    cleaned <- gsub("[,\\p{Zs}]+", "", raw, perl = TRUE)
    v <- suppressWarnings(as.numeric(cleaned))
    if (!is.finite(v)) return(NA_real_)
    v
  }
  
  c(pick("a"), pick("b"), pick("c"), pick("d"), pick("e"), pick("f"))
}

coupon_defaults <- c(0.00, 0.00, 0.0475, 0.12, 0.00, NA_real_)
wal_sched_defaults <- c(2.50008221571, 4.12450151726383, 4.15616438356164,
                        4.15616438356164, 4.15616438356164, NA_real_)
wal_unsched_defaults <- c(2.32, 4.00, 4.07, 4.02, 1.75, NA_real_)

sold_defaults <- c(0, 0, 1, 1, 0, 0)

rating_defaults <- c("AAA", "A+", "BBB-", "NR", "NR","")

build_tranche_df <- function(notionals) {
  letters_seq <- toupper(letters[1:6])
  data.frame(
    ClassID           = letters_seq,
    Tranche           = paste("Class", letters_seq),
    Notional          = as.numeric(notionals),
    Coupon            = coupon_defaults,
    WAL.scheduled     = wal_sched_defaults,
    WAL.unscheduled   = wal_unsched_defaults,
    Sold.default      = sold_defaults,
    Unfunded.default  = rep(FALSE, 6),      # NEW: add Unfunded.default column
    Rating            = rating_defaults,
    stringsAsFactors  = FALSE,
    check.names       = FALSE
  )
}


count_scenario_tranche_vectors <- function(scen_df) {
  nms <- .normalise_names(names(scen_df))
  sum(grepl("^class\\s*[a-z]\\s*balance$", nms))
}

# --- NEW helper: apply hot_col safely using numeric column index ---
.safe_hot_col <- function(ht, data, colname, ...) {
  idx <- match(colname, names(data))
  if (is.na(idx)) return(ht)
  rhandsontable::hot_col(ht, idx, ...)
}


# ---------------------------------------------------------------------
# mod95_server
# ---------------------------------------------------------------------
mod95_server <- function(id, bus) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    selected_scenario <- reactiveVal("sel_base")
    
    observeEvent(input$sel_base,       { if (isTRUE(input$sel_base))       selected_scenario("sel_base") })
    observeEvent(input$sel_stress,     { if (isTRUE(input$sel_stress))     selected_scenario("sel_stress") })
    observeEvent(input$sel_backloaded, { if (isTRUE(input$sel_backloaded)) selected_scenario("sel_backloaded") })
    observeEvent(input$sel_upload,     { if (isTRUE(input$sel_upload))     selected_scenario("sel_upload") })
    
    observeEvent(selected_scenario(), {
      s <- selected_scenario()
      updateCheckboxInput(session, "sel_base",       value = (s == "sel_base"))
      updateCheckboxInput(session, "sel_stress",     value = (s == "sel_stress"))
      updateCheckboxInput(session, "sel_backloaded", value = (s == "sel_backloaded"))
      updateCheckboxInput(session, "sel_upload",     value = (s == "sel_upload"))
    })
    
    scenario <- reactive({
      switch(
        selected_scenario(),
        sel_base       = "Base",
        sel_stress     = "Stress",
        sel_backloaded = "Stress_backloaded",
        sel_upload     = "Uploaded",
        "Base"
      )
    })
    
    scenario_df <- reactive({
      # ---- Uploaded scenario path (only when sel_upload is active) ----
      if (identical(selected_scenario(), "sel_upload")) {
        f <- input$scenario_file
        shiny::validate(
          shiny::need(!is.null(f) && !is.null(f$datapath) && file.exists(f$datapath),
                      "Please upload a scenario Excel file.")
        )
        
        # Prefer Sheet1; if not present, fall back to the first sheet
        sheets <- tryCatch(openxlsx::getSheetNames(f$datapath), error = function(e) character(0))
        sheet_to_use <- if ("Sheet1" %in% sheets) "Sheet1" else {
          shiny::validate(shiny::need(length(sheets) >= 1, "Uploaded file has no readable sheets."))
          sheets[1]
        }
        
        return(openxlsx::read.xlsx(f$datapath, sheet = sheet_to_use))
      }
      
      # ---- Existing behavior for built-in scenarios (unchanged) ----
      fname <- paste0(scenario(), ".xlsx")
      path  <- resolve_data_path(fname)
      shiny::validate(
        shiny::need(file.exists(path), paste("File not found:", path))
      )
      openxlsx::read.xlsx(path, sheet = "Sheet1")
    })
    
    output$scenario_preview <- renderTable(scenario_df(), rownames = FALSE)
    
    trancheDF_current <- reactive({
      n <- first_row_notionals(scenario_df())
      build_tranche_df(n)
    })
    
    observeEvent(scenario_df(), {
      df <- trancheDF_current()
      bus("tranche_df", df)
      bus("scenario_df", scenario_df())
      
      cat("\n[bus] tranche_df updated\n")
      cat("(mod95) published 'tranche_df' for scenario:", scenario(), "\n")
      print(df)
      cat("\n")
    }, ignoreInit = FALSE)
    
    output$tranche_tbl <- rhandsontable::renderRHandsontable({
      df <- bus("tranche_df")()
      if (is.null(df)) df <- trancheDF_current()
      req(df)
      
      # ensure required columns exist (no logic change)
      if (!"Rating" %in% names(df)) df$Rating <- "NR"
      if (!"Unfunded.default" %in% names(df)) df$Unfunded.default <- FALSE
      
      # SHOW order: Unfunded AFTER Rating
      keep <- c("Tranche","Notional","Coupon","WAL.scheduled",
                "WAL.unscheduled","Sold.default","Rating","Unfunded.default")
      keep <- keep[keep %in% names(df)]
      df_show <- df[, keep, drop = FALSE]
      
      ht <- rhandsontable::rhandsontable(
        df_show,
        rowHeaders = NULL,
        width = "100%",
        stretchH = "all",
        colHeaders = c("Tranche","Notional","Coupon",
                       "WAL (scheduled)","WAL (unscheduled)",
                       "Sold default","Rating","Unfunded Protection?")[seq_along(keep)]
      ) |>
        rhandsontable::hot_table(
          contextMenu = FALSE,
          readOnly = TRUE,
          stretchH = "all",
          manualColumnResize = FALSE
        ) |>
        rhandsontable::hot_cols(colWidths = 100)
      
    })
    
    
    observeEvent(input$tranche_tbl, {
      edited <- tryCatch(rhandsontable::hot_to_r(input$tranche_tbl), error = function(e) NULL)
      if (is.null(edited) || !is.data.frame(edited)) return()
      
      df_up <- bus("tranche_df")()
      if (is.null(df_up) || nrow(df_up) != nrow(edited)) return()
      
      # because tranche_tbl is a "view" without ClassID, we only sync editable fields by row position
      df_up$Tranche          <- as.character(edited$Tranche)
      df_up$Sold.default     <- suppressWarnings(as.numeric(edited$Sold.default))
      df_up$Rating           <- as.character(edited$Rating)
      df_up$Unfunded.default <- as.logical(edited$Unfunded.default)
      
      df_up$Rating[is.na(df_up$Rating)] <- ""
      df_up$Unfunded.default[is.na(df_up$Unfunded.default)] <- FALSE
      
      bus("tranche_df", df_up)
    }, ignoreInit = TRUE)
    
    
    
    mod90_summary <- bus("mod90_summary")
    output$dbg_mod90_summary <- renderPrint({
      x <- mod90_summary()
      if (is.null(x)) return(cat("(no payload yet)\n"))
      x
    })
    
    editor_df <- reactiveVal(NULL)
    
    output$tranche_editor_hot <- rhandsontable::renderRHandsontable({
      df <- editor_df()
      req(df)
      
      # make sure Rating column exists
      if (!"Rating" %in% names(df)) {
        df$Rating <- "NR"
      }
      
      # make sure Unfunded.default column exists
      if (!"Unfunded.default" %in% names(df)) {
        df$Unfunded.default <- FALSE
      }
      # DISPLAY order: Unfunded must be AFTER Rating
      df <- df[, c("ClassID","Tranche","Notional","Coupon","WAL.scheduled","WAL.unscheduled",
                   "Sold.default","Rating","Unfunded.default"), drop = FALSE]
      
      
      ht <- rhandsontable::rhandsontable(
        df,
        rowHeaders = NULL,
        useTypes = TRUE,
        stretchH = "all",
        width = "100%",
        colHeaders = c("ClassID","Tranche","Notional","Coupon",
                       "WAL (scheduled)","WAL (unscheduled)",
                       "Sold default","Rating","Unfunded Protection?")
      ) |>
        rhandsontable::hot_table(
          contextMenu = TRUE, manualRowMove = TRUE,
          allowInsertRow = FALSE, allowRemoveRow = FALSE, allowRowDrag = TRUE,
          rowHeaders = TRUE, stretchH = "all"
        ) |>
        rhandsontable::hot_cols(colWidths = 100)
    })
    
    
    output$remove_choice_ui <- renderUI({
      df <- editor_df()
      req(df)
      selectInput(ns("remove_choice"), "Select tranche to remove:", choices = df$Tranche, selected = df$Tranche[1])
    })
    
    output$add_choice_ui <- renderUI({
      canon <- trancheDF_current()
      present_ids <- if (!is.null(editor_df())) editor_df()$ClassID else character(0)
      avail <- canon[!(canon$ClassID %in% present_ids), ]
      if (nrow(avail) == 0) {
        selectInput(ns("add_choice"), "Select tranche to add:", choices = c("No tranche available" = ""), selected = "")
      } else {
        selectInput(ns("add_choice"), "Select tranche to add:", choices = setNames(avail$ClassID, avail$Tranche), selected = avail$ClassID[1])
      }
    })
    
    observeEvent(input$edit_tranches, {
      init <- bus("tranche_df")()
      if (is.null(init)) init <- trancheDF_current()
      editor_df(init)
      
      hide_css <- sprintf(
        "#%s .ht_clone_top th:nth-child(1), #%s .ht_master tr td:nth-child(1) { display: none !important; }",
        ns("tranche_editor_hot"), ns("tranche_editor_hot")
      )
      
      showModal(modalDialog(
        title = "Edit Tranches",
        size = "l",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_tranche_edits"), "Save", class = "btn-primary")
        ),
        tagList(
          tags$style(HTML(hide_css)),
          div(style = "margin-bottom:8px;",
              p("Edit tranche names and Sold %. Use the controls to add/remove a specific Class."),
              fluidRow(
                column(4, wellPanel(
                  h5("Add tranche"),
                  uiOutput(ns("add_choice_ui")),
                  actionButton(ns("add_tranche"), "Add selected", class = "btn btn-success", style = "margin-top:8px;")
                )),
                column(4, wellPanel(
                  h5("Remove tranche"),
                  uiOutput(ns("remove_choice_ui")),
                  actionButton(ns("remove_tranche"), "Remove selected", class = "btn btn-danger", style = "margin-top:8px;")
                )),
                column(4,
                       div(style = "padding: 8px 12px; background:#f8fafc; border-radius:6px; height:100%;",
                           tags$strong("Notes:"),
                           tags$ul(
                             tags$li("Add re-inserts the Class at its canonical position and preserves existing Sold edits for present Classes."),
                             tags$li("Remove hides the Class from the tranche list; click Save to publish changes."),
                             tags$li("You cannot add more Classes than exist in the scenario vectors.")
                           )
                       )
                )
              )
          ),
          tags$div(style = "height:12px;"),
          div(style = "width:100%;", rHandsontableOutput(ns("tranche_editor_hot"), height = "360px"))
        )
      ))
    })
    
    observeEvent(input$add_tranche, {
      df <- editor_df()
      req(df)
      sel <- input$add_choice
      if (is.null(sel) || sel == "") {
        showNotification("Select a valid tranche to add.", type = "error")
        return()
      }
      max_tr <- count_scenario_tranche_vectors(scenario_df())
      if (max_tr > 0 && nrow(df) >= max_tr) {
        showNotification("You can’t add more tranches than exist in the scenario vectors.", type = "error")
        return()
      }
      canon <- trancheDF_current()
      row_can <- canon[canon$ClassID == sel, , drop = FALSE]
      if (nrow(row_can) != 1) {
        showNotification("Selected tranche not found in canonical set.", type = "error")
        return()
      }
      present_ids <- df$ClassID
      new_ids <- c(present_ids, sel)
      ordered_all <- trancheDF_current()$ClassID
      keep_order <- ordered_all[ordered_all %in% new_ids]
      new_editor <- trancheDF_current()
      new_editor <- new_editor[new_editor$ClassID %in% keep_order, , drop = FALSE]
      for (i in seq_len(nrow(new_editor))) {
        id <- new_editor$ClassID[i]
        idx_old <- which(df$ClassID == id)
        if (length(idx_old) == 1) {
          new_editor$Tranche[i]      <- df$Tranche[idx_old]
          new_editor$Sold.default[i] <- df$Sold.default[idx_old]
          if ("Unfunded.default" %in% names(df)) {
            new_editor$Unfunded.default[i] <- df$Unfunded.default[idx_old]
          }
          if ("Rating" %in% names(df)) {
            new_editor$Rating[i] <- df$Rating[idx_old]
          }
        }
      }
      editor_df(new_editor)
      showNotification(sprintf("Added %s (will be published on Save).", row_can$Tranche), type = "message")
    })
    
    observeEvent(input$remove_tranche, {
      df <- editor_df()
      req(df)
      choice <- input$remove_choice
      if (is.null(choice) || !(choice %in% df$Tranche)) {
        showNotification("Select a valid tranche to remove.", type = "error")
        return()
      }
      df2 <- df[df$Tranche != choice, , drop = FALSE]
      editor_df(df2)
      showNotification(sprintf("Removed tranche '%s'. Click Save to publish changes.", choice), type = "message")
    })
    
    observeEvent(input$save_tranche_edits, {
      edited <- tryCatch(rhandsontable::hot_to_r(input$tranche_editor_hot), error = function(e) NULL)
      if (is.null(edited) || !is.data.frame(edited)) edited <- editor_df()
      if (!is.data.frame(edited)) {
        showNotification("No valid tranche table found. Please check your edits.", type = "error")
        return()
      }
      
      max_tr <- count_scenario_tranche_vectors(scenario_df())
      if (max_tr > 0 && nrow(edited) > max_tr) {
        showNotification("You can’t add more tranches than exist in the scenario vectors.", type = "error")
        return()
      }
      
      if (!("ClassID" %in% names(edited))) {
        internal <- editor_df()
        if (nrow(internal) == nrow(edited)) {
          edited$ClassID <- internal$ClassID
        } else {
          internal_map <- setNames(internal$ClassID, internal$Tranche)
          edited$ClassID <- ifelse(edited$Tranche %in% names(internal_map), internal_map[edited$Tranche], NA_character_)
        }
      }
      
      if (!("Tranche" %in% names(edited)) || !("Notional" %in% names(edited))) {
        showNotification("Edited table must include 'Tranche' and 'Notional' columns.", type = "error")
        return()
      }
      
      names(edited) <- gsub("^WAL \\(scheduled\\)$", "WAL.scheduled", names(edited))
      names(edited) <- gsub("^WAL \\(unscheduled\\)$", "WAL.unscheduled", names(edited))
      names(edited) <- gsub("^Sold\\.default$|^Sold default$", "Sold.default", names(edited))
      
      if (!"Coupon" %in% names(edited)) edited$Coupon <- NA_real_
      if (!"WAL.scheduled" %in% names(edited)) edited$WAL.scheduled <- NA_real_
      if (!"WAL.unscheduled" %in% names(edited)) edited$WAL.unscheduled <- NA_real_
      if (!"Sold.default" %in% names(edited)) edited$Sold.default <- NA_real_
      if (!"Rating" %in% names(edited)) edited$Rating <- NA_character_
      if (!"ClassID" %in% names(edited)) {
        canon_map <- setNames(trancheDF_current()$ClassID, trancheDF_current()$Tranche)
        edited$ClassID <- ifelse(edited$Tranche %in% names(canon_map), canon_map[edited$Tranche], NA_character_)
      }
      
      edited$Tranche      <- as.character(edited$Tranche)
      edited$Notional     <- suppressWarnings(as.numeric(gsub("[,\\s]", "", as.character(edited$Notional))))
      edited$Coupon       <- suppressWarnings(as.numeric(edited$Coupon))
      edited$WAL.scheduled   <- suppressWarnings(as.numeric(edited$WAL.scheduled))
      edited$WAL.unscheduled <- suppressWarnings(as.numeric(edited$WAL.unscheduled))
      edited$Sold.default <- suppressWarnings(as.numeric(edited$Sold.default))
      edited$ClassID      <- as.character(edited$ClassID)
      edited$Rating <- as.character(edited$Rating)
      edited$Rating[is.na(edited$Rating)] <- "" 
      # NEW: handle Unfunded.default
      if (!"Unfunded.default" %in% names(edited)) {
        edited$Unfunded.default <- FALSE
      }
      edited$Unfunded.default <- as.logical(edited$Unfunded.default)
      edited$Unfunded.default[is.na(edited$Unfunded.default)] <- FALSE
      
      
      if (any(is.na(edited$Tranche) | edited$Tranche == "")) {
        idx_empty <- which(is.na(edited$Tranche) | edited$Tranche == "")
        letters_seq <- toupper(letters[1:nrow(edited)])
        for (i in idx_empty) {
          edited$Tranche[i] <- paste("Class", letters_seq[i])
        }
      }
      
      canon_order <- trancheDF_current()$ClassID
      edited_ordered <- edited[order(match(edited$ClassID, canon_order)), , drop = FALSE]
      
      edited_ordered$Notional[is.na(edited_ordered$Notional)] <- 0
      
      bus("tranche_df", edited_ordered[, c("ClassID","Tranche","Notional",
                                           "Coupon","WAL.scheduled","WAL.unscheduled",
                                           "Sold.default","Unfunded.default","Rating")])
      removeModal()
      showNotification("Tranches updated and published to SRT module.", type = "message")
    })
    
    # ------------------------------------------------------------------
    # NEW: reset handler via bus (called from mod90 Reset)
    # ------------------------------------------------------------------
    observeEvent(bus("reset_srt")(), {
      # Reset scenario selection to initial defaults
      selected_scenario("sel_base")
      updateCheckboxInput(session, "sel_base",       value = TRUE)
      updateCheckboxInput(session, "sel_stress",     value = FALSE)
      updateCheckboxInput(session, "sel_backloaded", value = FALSE)
      
      # Rebuild and publish default scenario + tranche_df
      df <- try(trancheDF_current(), silent = TRUE)
      if (inherits(df, "try-error") || is.null(df)) return()
      
      bus("tranche_df", df)
      bus("scenario_df", scenario_df())
    }, ignoreInit = TRUE)
    
  })
}

# ================================
# Dummy parent app (standalone)
# ================================
options(shiny.dev.mode = TRUE)

ui <- fluidPage(
  titlePanel("Standalone Tranches Module — Dummy Parent"),
  sidebarLayout(
    sidebarPanel(
      mod95_ui_input("mod95")
    ),
    mainPanel(
      div(style = "margin-top: 5px"),
      mod95_ui_output("mod95")
    )
  )
)

server <- function(input, output, session) {
  bus <- bus_SRT()
  mod95_server("mod95", bus = bus)
}

shinyApp(ui, server)