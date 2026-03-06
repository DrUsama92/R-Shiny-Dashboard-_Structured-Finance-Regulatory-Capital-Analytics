################################################################################
# Global functions file contains the following functions:                      #
# -------------------------------------------------------                      #
# bus_create (LOA added Oct 10, 2025)                                          #
# bus_bind (LOA added Oct 20, 2025)                                            #
# bus_SRT (LOA added Oct 20, 2025)                                             #
# is_dev (LOA added Oct 20, 2025)                                              #
# mod90_css_href (LOA added Oct 20, 2025)                                      #
# resolve_data_path (LOA added Oct 21, 2025)                                   #
# button_01                                                                    #
# button_02                                                                    #
# button_03                                                                    #
# formatValue                                                                  #
# pct                                                                          #
# removeCommas                                                                 #
# filenameMapFun                                                               #
# numInputFun                                                                  #
# plotFun                                                                      #
# saveUploadFun                                                                #
# pctFun                                                                       #
# tooltipFun                                                                   #
################################################################################

# LOA added bus_create and bus_bind Oct 10 2025

# ------------------------------------------------------------------------------
# bus_create(...)
# ------------------------------------------------------------------------------
# A lightweight, per-session publish/subscribe message bus for Shiny modules.
#
# WHAT IT IS
#   - A single object (the “bus”) you pass to sibling modules so they can
#     communicate without referencing each other directly.
#   - Topics are named channels (e.g., "class_df", "mod90_summary").
#   - Each topic stores a reactive value; subscribers get a reactive() stream,
#     publishers set the latest value.
#
# WHY USE IT
#   - Decouples modules (better reuse & testability).
#   - Strong scoping (one bus per Shiny session; no globals or cross-user leaks).
#   - Stable signatures: modules stay `modX_server(id, bus)` as you add more data.
#   - Centralized control: validation, allowlisting, and logging in one place.
#
# HOW IT WORKS
#   - Call `bus_create()` in the parent server to create a per-session bus.
#   - The returned object is a single callable function:
#       * SUBSCRIBE (read):  bus("topic")              -> returns reactive()
#       * PUBLISH   (write): bus("topic", value)       -> sets topic’s value
#     The subscribe path always returns a reactive() that yields *plain values*.
#     Do NOT publish reactive objects; publish the values from a reactive.
#
# REACTIVE SOURCES
#   - To wire a reactive source into a topic, publish its *values* when it changes.
#     A helper like `bus_bind()` (see below) is convenient:
#       observeEvent(src(), ignoreInit = FALSE, { bus("topic", src()) })
#
# PARAMETERS
#   allowed    : character vector of allowed topic names (NULL = allow all).
#   validators : named list of functions, one per topic, called as f(value).
#                Use them to enforce schemas; should `stop()` on failure.
#   logger     : optional function(topic, value) called on every publish.
#
# EXAMPLE (parent server):
#   bus <- bus_create(allowed = c("class_df","mod90_summary"))
#   mod95_server("mod95", bus = bus)  # publishes "class_df"
#   mod90_server("mod90", bus = bus)  # subscribes "class_df", publishes "mod90_summary"
#
# EXAMPLE (sender module):
#   df_rx <- reactive({ data.frame(x = 1:3) })
#   observeEvent(df_rx(), ignoreInit = FALSE, { bus("class_df", df_rx()) })
#
# EXAMPLE (receiver module):
#   class_df <- bus("class_df")  # reactive()
#   observeEvent(class_df(), { print(class_df()) }, ignoreInit = FALSE)
#
# PITFALLS / TIPS
#   - Don’t publish reactive objects themselves (avoid reactive-of-reactive).
#   - Keep topic names centralized to avoid typos.
#   - Validators are your friend for schema/shape checks.
#   - The bus is per-session; create it inside the parent `server()`.
# ------------------------------------------------------------------------------
# Function to install + load packages safely
safe_lib <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# --- Packages Needed ---
safe_lib("openxlsx")
safe_lib("rhandsontable")
safe_lib("dplyr")      # if needed
safe_lib("shiny")      # if needed
safe_lib("shinyjs")    # if needed


bus_create <- function(allowed = NULL, validators = list(), logger = NULL) {
  topics <- new.env(parent = emptyenv())
  
  ensure <- function(name) {
    if (is.null(topics[[name]])) {
      rv <- shiny::reactiveVal(NULL)
      topics[[name]] <- list(
        get = shiny::reactive(rv()),
        set = function(x) rv(x)
      )
    }
    topics[[name]]
  }
  
  validate_topic <- function(name) {
    if (!is.null(allowed) && !(name %in% allowed))
      stop(sprintf("Topic '%s' not allowed.", name), call. = FALSE)
  }
  
  validate_value <- function(name, value) {
    v <- validators[[name]]
    if (is.function(v)) v(value)  # should stop() on failure
  }
  
  # Single callable API:
  #   bus("topic")            -> reactive()   (subscribe)
  #   bus("topic", value)     -> invisible()  (publish)
  function(name, value) {
    validate_topic(name)
    t <- ensure(name)
    if (missing(value)) {
      t$get
    } else {
      validate_value(name, value)
      if (is.function(logger)) logger(name, value)
      t$set(value)
      invisible(TRUE)
    }
  }
}

# ------------------------------------------------------------------------------
# bus_bind(bus, topic, src_reactive, transform = identity, priority = 0)
# Bind a reactive source to a bus topic; publishes the source's *values*
# whenever it changes.
# ------------------------------------------------------------------------------
bus_bind <- function(bus, topic, src_reactive, transform = identity, priority = 0) {
  observeEvent(src_reactive(), {
    bus(topic, transform(src_reactive()))
  }, ignoreInit = FALSE, priority = priority)
}

# LOA added bus_SRT Oct 20, 2025
# =====================================================================
# bus_SRT(...)
# ---------------------------------------------------------------------
# Purpose:
#   Create a per-session pub/sub "bus" used by the SRT app’s modules
#   (e.g., mod90 and mod95) to exchange data without tight coupling.
#   The bus exposes named topics; modules publish to and/or subscribe
#   from those topics using the same bus instance.
#
# What this initializes:
#   • allowed topics (the only topics that can be used)
#       - "tranche_df"    : data.frame of tranche rows (source: mod95)
#                           REQUIRED columns: Tranche, Notional
#                           OPTIONAL columns: Coupon, WAL.scheduled,
#                                            WAL.unscheduled, etc.
#       - "mod90_summary" : data.frame summary (source: mod90)
#                           REQUIRED columns: n_tranches, notional_total
#
#   • validators:
#       Lightweight guards that enforce payload shape per topic.
#       They keep modules honest and surface integration errors early.
#
#   • logger (optional):
#       A callback called on publish(topic, value) for quick tracing.
#       Defaults to printing "[bus] <topic> updated".
#
# When to use:
#   - In the Master Parent App and in each dummy parent app, call
#         bus <- bus_SRT()
#     and pass that same 'bus' into each module:
#         mod95_server("mod95", bus = bus)
#         mod90_server("mod90", bus = bus)
#
# How modules interact:
#   - Subscribe:  my_df <- bus("tranche_df")  # returns reactive()
#                  observe({ print(my_df()) })
#   - Publish:    bus("tranche_df", new_df)
#
# Why this exists:
#   Centralizes the bus configuration so all app entry points share the
#   same topics + validation rules. This avoids copy/paste drift and
#   keeps the Master light (traffic-cop only).
#
# Extending topics (guidelines):
#   1) Add the topic name to 'allowed'.
#   2) Add a validator that is strict on REQUIRED columns/shape and
#      permissive on OPTIONAL fields.
#   3) Document the topic, producer, and expected shape in the header.
#   4) Keep topic names short, specific, and module-agnostic if possible.
#
# Example (Master or dummy parent):
#   bus <- bus_SRT()
#   mod95_server("mod95", bus = bus)  # publishes 'tranche_df'
#   mod90_server("mod90", bus = bus)  # subscribes 'tranche_df', publishes 'mod90_summary'
#
# Notes:
#   • This returns a *bus function*; calling bus("topic") gives a reactive
#     subscription; calling bus("topic", value) publishes.
#   • The bus is per-session. Create it inside the Shiny server function.
# =====================================================================
bus_SRT <- function(
    logger = function(topic, value) cat(sprintf("[bus] %s updated\n", topic))
) {
  bus_create(
    allowed = c(
      "tranche_df",      # mod95 -> mod90
      "mod90_summary",   # mod90 -> mod95 (optional)
      "capital_summary", # mod90 -> others (optional)
      "scenario_df",     # NEW: full Sheet1 of Base/Stress/Backloaded
      "amort_df",        # NEW: optional amortisation table, if we use it
      "reset_srt"        # NEW: factory reset trigger for SRT area
    ),
    validators = list(
      tranche_df = function(x) {
        stopifnot(is.data.frame(x))
        req_core <- c("Tranche", "Notional")
        stopifnot(all(req_core %in% names(x)))
        invisible(TRUE)
      },
      mod90_summary = function(x) {
        stopifnot(is.data.frame(x),
                  all(c("n_tranches", "notional_total") %in% names(x)))
        invisible(TRUE)
      },
      capital_summary = function(x) {
        stopifnot(is.list(x),
                  all(c("scope","E26","G26","D16") %in% names(x)))
        invisible(TRUE)
      },
      scenario_df = function(x) {          # NEW
        stopifnot(is.data.frame(x))
        # At minimum it must have a Period column, like the XLS
        stopifnot(any(tolower(names(x)) == "period"))
        invisible(TRUE)
      },
      amort_df = function(x) {             # NEW (for the Amortisation tab)
        stopifnot(is.data.frame(x))
        stopifnot(any(tolower(names(x)) == "period"))
        invisible(TRUE)
      },
      reset_srt = function(x) {            # NEW: simple logical flag, no payload requirements
        invisible(TRUE)
      }
    ),
    logger = logger
  )
}


# ─────────────────────────────────────────────────────────────────────────────
# Shared helpers & capital defaults used by mod90 (safe to define once here)
# ─────────────────────────────────────────────────────────────────────────────

# Null-coalescing helper used throughout mod90:
`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

# Percent → decimal; robust to NULL/NA/empty:
from_pct_num <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (!length(v) || !is.finite(v)) return(0)
  v / 100
}

# Capital defaults (DECIMALS). These mirror the yellow inputs in Excel.
.cap_defaults_group <- list(
  distribution = c(0.1196, 0.0150, 0.0200, 0.1314),
  cost         = c(0.1215, 0.0729, 0.0333, 0.0183)
)
.cap_defaults_local <- list(
  distribution = c(0.1226, 0.0150, 0.0200, 0.0658),
  cost         = c(0.1215, 0.0729, 0.0333, 0.0183)
)



# LOA added is_dev and mod90_css_href Oct 20 2025
# Below are cache-buster automatic and environment-aware functions for changes to CSS file during development
# ---- Dev / Prod switch ---------------------------------------------
# Turn on "dev" with either:
#   options(shiny.dev.mode = TRUE)
# or Sys.setenv(SHINY_DEV_MODE = "true")
is_dev <- function() {
  isTRUE(getOption("shiny.dev.mode")) ||
    identical(Sys.getenv("SHINY_DEV_MODE"), "true")
}

# ---- CSS href builder (cache-bust in dev, stable in prod) ----------
# Looks for mod90.css in common places; bumps ?v when the file changes (dev),
# and uses a fixed version tag in prod (change "1.0.0" when you release).
mod90_css_href <- function() {
  base_name <- "mod90.css"
  # Common search paths so it works in both Master and dummy module apps
  candidates <- c(file.path("www", base_name), base_name)
  path <- candidates[file.exists(candidates)]
  path <- if (length(path)) path[[1]] else candidates[[1]]
  
  if (is_dev()) {
    # mtime-based version so it only changes when the file changes
    ver <- tryCatch(
      as.integer(file.info(path)$mtime),
      error = function(e) as.integer(Sys.time())
    )
    sprintf("%s?v=%s", base_name, ver)
  } else {
    # Production: stable version (keep browser caching strong).
    # Bump this string when you ship new CSS.
    sprintf("%s?v=%s", base_name, "1.0.0")
  }
}

# LOA added below Oct 21, 2025
# =====================================================================
# resolve_data_path(fname)
# ---------------------------------------------------------------------
# Returns a readable path for a data file.
# Prefers ./data/<fname>; falls back to absolute path.
# Keeps mod code clean and identical between Master and dummies.
# =====================================================================
resolve_data_path <- function(fname) {
  rel <- file.path("data", fname)
  if (file.exists(rel)) return(rel)
  # fallback (unchanged)
  abs <- file.path("C:/Users/Maham/OneDrive/Desktop/App/data", fname)
  return(abs)
}


button_01 <- function(x, y) {
  actionButton(
    x,
    label = div(
      style = "display: inline-flex; align-items: top;",
      div(style = "padding-right: 5px;", icon("info-circle")),  # Icon to the left
      span(style = "white-space: normal;", y)  # Text to the right
    ),
    style = "background-color:transparent;
             border:none;
             width:100%;
             margin-top:-5px;
             margin-bottom:5px;
             margin-left:0px;
             padding:0;
             text-align:left;
             white-space: normal;
             word-wrap: break-word;"
  )
}

button_02 <- function(x,y){actionButton(x,y,
                                        icon =   icon("info-circle"),
                                        style =  "text-align:left;
                      background-color:DarkGrey;
                      color:white;
                      width:100%;
                      margin-bottom:4px;          
                      border-color:DarkGrey")}

button_03 <- function(inputId,
                      label,
                      ns = identity,
                      width = NULL,
                      min_width = "70px",
                      full_width = FALSE,
                      additional_style = "",
                      type = c("action", "download"),
                      ...) {
  
  type <- match.arg(type)
  
  # Core button styles
  style <- paste0(
    "box-shadow: 2px 2px 4px rgba(0,0,0,0.2); ",
    "border: 1px solid #d0d0d0; ",
    "transition: all 0.2s ease; ",
    "min-width: ", min_width, "; ",
    if (!is.null(width)) paste0("width: ", width, "; ") else "",
    if (full_width) "width: 100%; " else "",
    additional_style
  )
  
  # Button creation
  btn <- if (type == "download") {
    downloadButton(outputId = ns(inputId), label = label, style = style, ...)
  } else {
    actionButton(inputId = ns(inputId), label = label, style = style, ...)
  }
  
  # Return with embedded CSS
  tagList(
    singleton(tags$head(
      tags$style(HTML("
        .pop-forward {
          display: inline-block;
          transition: transform 0.2s ease, box-shadow 0.2s ease;
        }
        .pop-forward:hover {
          transform: scale(1.05);
        }
      "))
    )),
    tags$div(
      class = "pop-forward",
      style = if (full_width) "display: block;" else NULL,
      btn
    )
  )
}


# Format values 
formatValue <- function(x){format(round(as.numeric(x),2),nsmall=2,big.mark=",")} # format values
pct <- function(x){paste(format(round(x*100,digits=1),nsmall=1),"%",sep="")} # convert to percentage
removeCommas<-function(x){x<-as.numeric(gsub("\\,", "", x))}  

# Derive save file names when downloading
filenameMapFun <- function(choices) {
  setNames(
    paste(
      gsub(" ", "_", choices), # replaces whitespaces with "_" underscores
      # Below assigns file type based on name flags, will need to be adapted as code progresses
      ifelse(grepl("plot|graph|chart|image", choices, ignore.case = TRUE), "png", "csv"), 
      sep = "."
    ),
    choices
  )
}

# Numeric input function using shinyWidgets::autonumericInput(...) with no server interaction
numInputFun <- function(
    # --- AutoNumeric parameters ---
  inputId,
  value = 60,
  decimalPlaces = 0,
  minimumValue = 1,
  maximumValue = 360,
  currency_symbol = NULL,
  currency_symbol_placement = NULL,
  digit_group_separator = NULL,
  symbol_when_unfocused = NULL,
  
  # --- NEW: require confirmation before sending value ---
  accept_input = FALSE,   # Add confirmation checkmark if TRUE
  
  # --- UI styling ---
  input_height = "28px",
  input_font_size = "13px",
  font_size = "13px",
  width = "70px",
  label_text = "Label goes here:",
  label_tooltip = NULL,
  label_color = "#6c757d",
  label_bold = FALSE,
  background = TRUE,
  container_height = "34px",
  
  # --- Shiny infra ---
  ns = identity,
  session = shiny::getDefaultReactiveDomain()
){
  if (missing(inputId)) stop("inputId is required")
  
  # Add pop-forward CSS once
  css_key <- paste0("pop_forward_css_added_", ns(""))
  insert_css <- NULL
  if (is.null(session$userData[[css_key]])) {
    insert_css <- shiny::singleton(shiny::tags$style(shiny::HTML("
      .pop-forward { transition: transform 0.2s ease; }
      .pop-forward:hover { transform: scale(1.05); z-index: 2; }
      .confirm-btn-small {
        margin-left: 4px;
        padding: 0px 4px;
        font-size: 12px;
        height: 22px;
        line-height: 20px;
      }
    ")))
    session$userData[[css_key]] <- TRUE
  }
  
  # Build label
  label <- if (!is.null(label_tooltip)) {
    tooltipFun(label_text, label_tooltip, bold = label_bold)
  } else {
    shiny::tags$span(
      style = paste0(
        "font-size:", font_size, ";",
        "color:", label_color, ";",
        if (label_bold) "font-weight:bold;" else "",
        "display:flex;align-items:center;"
      ),
      label_text
    )
  }
  
  # Container styling
  container_style <- paste0(
    if (background) "background-color:#f0f0f0;" else "",
    "padding:4px 10px; border-radius:4px;",
    "display:flex; align-items:center; gap:10px;",
    "height:", container_height, ";"
  )
  
  # Common autonumeric options
  args <- list(
    inputId = ns(inputId),
    label = NULL,
    value = value,
    decimalPlaces = decimalPlaces,
    minimumValue = minimumValue,
    maximumValue = maximumValue,
    step = 1,
    width = width,
    modifyValueOnWheel = TRUE,
    style = paste0(
      "height:", input_height, ";",
      "font-size:", input_font_size, ";",
      "margin:0; padding:0 4px;",
      "border:1px solid #ccc; border-radius:4px;",
      "line-height:", input_height, ";",
      "text-align:center;"
    ),
    options = list(caretPositionOnFocus = "start")
  )
  
  # Add optional formatting params
  if (!is.null(currency_symbol)) args$currencySymbol <- currency_symbol
  if (!is.null(currency_symbol_placement)) args$currencySymbolPlacement <- currency_symbol_placement
  if (!is.null(digit_group_separator)) args$digitGroupSeparator <- digit_group_separator
  if (!is.null(symbol_when_unfocused)) args$symbolWhenUnfocused <- symbol_when_unfocused
  
  # Create the AutoNumeric input
  input_box <- do.call(shinyWidgets::autonumericInput, args) |>
    shiny::tagAppendAttributes(style = "margin:0;")
  
  # If accept_input = FALSE → return original layout
  if (!accept_input) {
    return(shiny::tagList(
      insert_css,
      shiny::div(
        style = container_style,
        label,
        shiny::div(class = "pop-forward", input_box)
      )
    ))
  }
  
  # If accept_input = TRUE → add small checkmark button
  confirm_button <- shiny::actionButton(
    inputId = ns(paste0(inputId, "_confirm")),
    label = "✔",
    class = "confirm-btn-small"
  )
  
  # ALSO ADD A HIDDEN FIELD TO HOLD THE CONFIRMED VALUE
  hidden_confirm <- shiny::tags$script(
    sprintf("
    $(document).on('click', '#%s', function() {
      var raw = $('#%s').val();
      Shiny.setInputValue('%s', raw);
    });
  ",
            ns(paste0(inputId, "_confirm")),     # button
            ns(inputId),                         # raw autonumeric input
            ns(paste0(inputId, "_confirmed"))    # confirmed raw value
    )
  )
  
  shiny::tagList(
    insert_css,
    shiny::div(
      style = container_style,
      label,
      shiny::div(class = "pop-forward", input_box),
      confirm_button
    ),
    hidden_confirm
  )
}


plotFun <- function(data, title, y_axis = "percent", show_fitted = FALSE, grid_mode = FALSE) {
  # Remove NA/Inf values
  data <- data[is.finite(data[, 1]), , drop = FALSE]
  
  # Standardize data structure
  if (!show_fitted) {
    data <- data.frame(X = seq_along(data[, 1]), Y = data[, 1])
  }
  
  # Adjust title size for grid layout if needed
  title_size <- if (grid_mode) 13 else 16
  
  # Base plot
  p <- ggplot(data, aes(x = X)) +
    labs(title = title, x = "Period", y = "") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 12),
      plot.margin = unit(c(2, 1, 1, 0), "lines"),
      plot.title = element_text(size = title_size, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      axis.text = element_text(size = 10)
    ) +
    scale_y_continuous(
      labels = if (y_axis == "amounts") scales::label_comma(accuracy = 1) else pctFun
    )
  
  # Conditional layers
  if (show_fitted) {
    p <- p +
      geom_point(aes(y = Original, color = "Original"), size = 2) +
      geom_line(aes(y = Original, color = "Original"), linetype = "solid", linewidth = 1) +
      geom_line(aes(y = Fitted, color = "Fitted"), linetype = "dashed", linewidth = 1) +
      scale_color_manual(values = c("Original" = "blue", "Fitted" = "red")) +
      theme(legend.position = "bottom")
  } else {
    p <- p +
      geom_point(aes(y = Y), color = "blue", size = 2) +
      geom_line(aes(y = Y), color = "blue", linetype = "solid", linewidth = 1) +
      theme(legend.position = "none")
  }
  return(p)
}

# For saving/downloading and uploading objects. User can save/upload from/to server
# or local machine (download directory). Should accomodate text, dataframes, lists, matrixes
# but will be ongoing developed. 
# Added July 12, 2025
saveUploadFun <- function(
    input,                                   # Shiny input object (passed from server())
    session,                                 # Shiny session object (passed from server())  
    object_to_save,                          # Reactive() or static object to be saved (e.g., reactive(df), reactive(input$text_input), etc.)
    save_trigger_id,                         # UI ID of the save action button in UI (e.g., "save_btn")
    upload_trigger_id,                       # UI ID of the upload action button in UI (e.g., "upload_btn")          
    default_name = paste0(                   # Default filename (no extension), e.g., "data_YYYYMMDD"    
      "data_", format(Sys.time(), "%Y%m%d")  
    ),
    user_dir = "./user_saves/user_123/",     # Server folder path where .rds files are stored (must exist or be created)
    accepted_classes = NULL,                 # Optional vector of accepted R classes (e.g., c("data.frame", "matrix", "list")); NULL skips class check
    assign_callback = NULL,                  # Function receiving uploaded object and assigning it back into the app; return NULL or FALSE to cancel success
    file_list = reactiveVal(NULL),           # reactiveVal() holding list of available server files (for populating server file dropdown)
    refresh_file_list = NULL,                # Function to refresh the file_list after saving or deleting   
    extension = ".rds",                      # File extension, default ".rds"  
    filename_prefix = "",                    # Optional filename prefix (e.g., "project_" → "project_data_20250710.rds")
    validation_callback = NULL               # Optional extra validation function to apply stricter checks to uploaded object; should return TRUE/FALSE
) {
  
  ns <- session$ns
  
  # NEW (safe): ensure server dir exists
  if (!dir.exists(user_dir)) dir.create(user_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ───────────────
  # Helper functions
  # ───────────────
  get_input <- function(id) input[[id]]
  validate_filename <- function(name) grepl("^[A-Za-z0-9_-]+$", name)
  timestamp_suffix <- function() format(Sys.time(), "%y%m%d%H%M%S")
  server_file_path <- function(name) fs::path(user_dir, name)
  
  make_filename <- function(base, ext = extension, use_timestamp = FALSE) {
    if (use_timestamp) {
      paste0(filename_prefix, base, ".", timestamp_suffix(), ext)
    } else {
      paste0(filename_prefix, base, ext)
    }
  }
  
  is_valid_upload <- function(obj) {
    if (!is.null(accepted_classes) && !any(class(obj) %in% accepted_classes)) {
      showNotification(paste("Invalid object type. Must be one of:", paste(accepted_classes, collapse = ", ")), type = "error")
      return(FALSE)
    }
    if (!is.null(validation_callback)) {
      result <- validation_callback(obj)
      if (!is.null(result) && !result) {
        showNotification("Uploaded object failed validation checks.", type = "error")
        return(FALSE)
      }
    }
    TRUE
  }
  
  # ───────────────
  # SAVE modal + logic
  # ───────────────  
  observeEvent(input[[save_trigger_id]], {
    showModal(modalDialog(
      title = "Save Options",
      tagList(
        radioButtons(ns("save_choice"), "Save to:",
                     choices = c("Server" = "server", "Local Machine" = "local"), selected = "server"),
        textInput(ns("save_input_name"), "Filename (without extension):", value = default_name)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("confirm_save"), "Save", class = "btn-primary")
      ),
      size = "m",
      easyClose = FALSE
    ))
  })
  
  # --- Inside saveUploadFun(), replace ONLY the "else { ... }" branch under confirm_save ---
  
  observeEvent(get_input("confirm_save"), {
    req(get_input("save_input_name"))
    if (!validate_filename(get_input("save_input_name"))) {
      showNotification("Invalid filename. Use only letters, numbers, '-', or '_'.", type = "error")
      return()
    }
    
    # IMPORTANT CHANGE:
    # "Local Machine" will now behave exactly like "Server":
    # it saves to the SAME server folder (user_dir).
    filename <- make_filename(get_input("save_input_name"))
    
    saveRDS(object_to_save(), file = server_file_path(filename))
    if (!is.null(refresh_file_list)) refresh_file_list()
    
    # Optional: tweak message depending on choice (keeps UI semantics)
    if (get_input("save_choice") == "server") {
      showNotification(paste("Saved to server as:", filename), type = "message")
    } else {
      showNotification(paste("Saved (Local Machine option) to server folder as:", filename), type = "message")
    }
    
    removeModal()
  })
  
  # ───────────────
  # UPLOAD modal + logic
  # ───────────────  
  observeEvent(input[[upload_trigger_id]], {
    showModal(modalDialog(
      title = "Upload Options",
      tagList(
        radioButtons(ns("upload_choice"), "Upload from:",
                     choices = c("Server" = "server", "Local Machine" = "local"), selected = "server"),
        conditionalPanel(
          condition = paste0("input['", ns("upload_choice"), "'] == 'server'"),
          if (length(file_list()) == 0) {
            p("No files available on server.")
          } else {
            selectInput(ns("file_choice"), "Select file:", choices = file_list())
          }
        ),
        conditionalPanel(
          condition = paste0("input['", ns("upload_choice"), "'] == 'local'"),
          fileInput(ns("local_file_upload"), "Choose .RDS file:", accept = ".rds", width = "100%")
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("confirm_upload"), "Upload", class = "btn-primary")
      ),
      size = "m",
      easyClose = FALSE
    ))
  })
  
  # Handle upload: from Local Machine  
  observeEvent(get_input("local_file_upload"), {
    req(get_input("local_file_upload"))
    tryCatch({
      obj <- readRDS(get_input("local_file_upload")$datapath)
      if (!is_valid_upload(obj)) return()
      if (!is.null(assign_callback)) {
        ok <- assign_callback(obj)
        if (identical(ok, FALSE)) return()  # NEW (safe)
      }
      showNotification("Uploaded from local file!", type = "message")
      removeModal()
    }, error = function(e) {
      showNotification("Error loading file. Please check the file format.", type = "error")
    })
  })
  
  # Handle upload: from Server
  observeEvent(get_input("confirm_upload"), {
    if (get_input("upload_choice") != "server") return()
    req(get_input("file_choice"))
    tryCatch({
      obj <- readRDS(server_file_path(get_input("file_choice")))
      if (!is_valid_upload(obj)) return()
      if (!is.null(assign_callback)) {
        ok <- assign_callback(obj)
        if (identical(ok, FALSE)) return()  # NEW (safe)
      }
      showNotification("Loaded from server!", type = "message")
      removeModal()
    }, error = function(e) {
      showNotification("Error loading file. Please check the file format.", type = "error")
    })
  })
}
# End bundled save-upload function

# Function for formatting percentages
pctFun <- function(x) {paste0(formatC(x * 100, format = "f", digits = 0), "%")}

# Standard tooltip format to use throughout
tooltipFun <- function(label, tooltip_html, icon_size = "12px", bold = FALSE) {
  tagList(
    singleton(tags$head(tags$style(HTML('
      .shiny-tooltip-container {
        position: relative;
        display: inline-flex;
        align-items: center;
        gap: 4px;
        cursor: help;
      }
      .tooltip-icon {
        color: #6c757d;
      }
      .shiny-tooltip {
        visibility: hidden;
        width: 200px;
        background-color: #f8f9fa;
        color: #212529;
        text-align: justify;
        font-size: 12px;
        font-weight: normal;  /* default: not bold */
        padding: 8px 12px;
        border-radius: 4px;
        border: 1px solid #dee2e6;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        position: absolute;
        z-index: 9999;
        margin-top: 5px;
        top: 100%;
        left: 0;
      }
      .shiny-tooltip-container:hover .shiny-tooltip {
        visibility: visible;
        opacity: 1;
      }
      .shiny-tooltip ul {
        margin: 5px 0;
        padding-left: 20px;
      }
    ')))),
    
    tags$span(
      class = "shiny-tooltip-container",
      tags$span(class = "tooltip-label", label),
      tags$span(
        class = "tooltip-icon",
        style = paste0("font-size: ", icon_size, ";"),
        "\u2754"
      ),
      tags$div(
        class = "shiny-tooltip",
        style = if (bold) "font-weight: bold;" else NULL,
        HTML(tooltip_html)
      )
    )
  )
}