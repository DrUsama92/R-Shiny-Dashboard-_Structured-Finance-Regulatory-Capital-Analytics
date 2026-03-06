#MOD90.R
#library(dplyr)
#library(rhandsontable)
#library(shiny)
#library(shinyjs)

source("./GlobalText.R")
source("./Mod95.R")

### NEW: Global helpers are actually used throughout this file
source("./GlobalFunctions.R") # numInputFun, button_03, saveUploadFun, mod90_css_href, bus_SRT, etc.
source(file.path("mod90_support", "KeysSheet.R"), local = TRUE)
source(file.path("mod90_support", "TableUIHelpers.R"), local = TRUE)
source(file.path("mod90_support", "ERBA.R"), local = TRUE)
source(file.path("mod90_support", "EconCore.R"), local = TRUE)
source(file.path("mod90_support", "SEC-IRBA.R"), local = TRUE)
source(file.path("mod90_support", "Output.R"), local = TRUE)



### NEW: safe default used when periods not yet set
if (!exists("mosBase")) mosBase <- 60

######################################################################################
## Below is module for SRT                                                          ##
######################################################################################


mod90_ui_input <- function(id) {
  ns <- NS(id)
  
  # Module-scoped helper function for in-line buttons in sidebar
  button_group <- function(..., margin_top = "10px") {
    div(
      style = sprintf("margin-top: %s;", margin_top),
      fluidRow(
        div(
          style = "margin: 0 15px; display: flex; justify-content: center; gap: 6px;",
          ...
        )
      )
    )
  }
  
  # Module-scoped helper function for checkbox inputs in sidebar
  checkbox_custom <- function(inputId, label, value = FALSE, margin_left = "5px") {
    div(class = "pop-forward", 
        style = sprintf("display:block; width:100%%; margin-left:%s;", margin_left),
        checkboxInput(ns(inputId), label, value))
  }
  
  tagList(
    singleton(
      tags$head(
        ### CSS file saved in mod90.css under /www directory ###
        tags$link(rel = "stylesheet", type = "text/css", href = mod90_css_href()), # draws on Global Function
        
        # -------------------------------------------------------------------
        # NEW: Professional table headings for Economics & ERBA
        # -------------------------------------------------------------------
        tags$style(HTML("
      /* Professional table headings for Economics & ERBA */
      .table-heading {
        font-size: 17px;              /* bigger, report-style */
        font-weight: 700;             /* strong emphasis */
        color: #1F2937;               /* dark professional gray */
        margin-top: 26px;
        margin-bottom: 10px;
        padding-left: 2px;
        letter-spacing: 0.2px;
      }

      /* Optional: subtle divider line under heading */
      .table-heading::after {
        content: '';
        display: block;
        margin-top: 6px;
        width: 48px;
        border-bottom: 2px solid #D1D5DB;
      }

      /* Section-level headings (optional) */
      .econ-section-heading {
        font-size: 19px;
        font-weight: 800;
        margin-top: 34px;
        margin-bottom: 14px;
        color: #111827;
      }
    ")),
        
        
        ### JavaScript files saved under /www directory ###
        tags$script(src = "jquery-ui.min.js"), # draggable modal js loads from local file under www/
        tags$script(src = "mod90.js"), # main JS file containing all custom logic
        tags$script(src = "download-handler.js"),
        
        # Bind toggle (calls helper defined inside mod90.js)
        tags$script(HTML(sprintf(
          "window.mod90 && mod90.bindToggle('%s','%s');",
          ns('opt_inputs'), ns('manualInputBox')
        )))
        
      )
    ),
    
    shinyjs::useShinyjs(),
    
    # Sidebar header owned by mod90
    fluidRow(
      helpText(
        h5(strong("Base Input Panel")),
        align = "center",
        style = "margin-top:-15px; margin-bottom:5px"
      )
    ),
    
    div(
      class = "pop-forward", 
      style = "display:block; width:100%; margin-bottom: 10px",
      button_02(ns("explain1010"), "SRT section explained")
    ),
    
    
    div(
      style = "border: 1px solid #e0e0e0; border-radius: 4px; padding: 4px 4px; background: #f8fafc;",
      numInputFun(
        inputId = "periods",
        accept_input = TRUE,
        input_height = "28px",
        container_height = "34px",
        input_font_size = "13px",
        font_size = "14px",
        label_text = "Periods (x-axis)",
        label_tooltip = "To enter a value:
                <ul>
                  <li>Highlight the existing value and overwrite it; or</li>
                  <li>Highlight the existing value and use mouse wheel to scroll the value up or down.</li>
                  <li>Note that invalid inputs, such as decimals or letters, will be ignored.</li>
                </ul>
                ",
        background = FALSE,
        ns = ns
      )
    ),
    
    button_group( 
      button_03("resetMod", "Reset", ns = ns, width = "76px", min_width = "0"),
      button_03("modal_upload", "Upload", ns = ns, width = "76px", min_width = "0"), 
      button_03("save_btn", "Save", ns = ns, type = "action", width = "76px", min_width = "0")
    ),
    
    # =======================================================================
    # MAIN SIDEBAR GROUPS (reordered & restructured)
    # =======================================================================
    
    ## 1) PORTFOLIO (main group)
    tags$hr(),
    h4("Portfolio"),
    div(
      id = ns("portfolioGroup"),
      # start collapsed (unchecked) on load
      checkbox_custom("portfolioInputs", "Portfolio inputs", FALSE),
      
      conditionalPanel(
        condition = paste0("input['", ns("portfolioInputs"), "'] == true"),
        div(
          id = ns("portfolioChildren"),
          class = "keys-card",
          style = "margin-top:4px; padding:8px 10px; background:#f9fafb; border:1px solid #e5e7eb; border-radius:6px;",
          
          # Core parameters subgroup (collapsed by default)
          checkbox_custom("coreParams", "Core parameters", FALSE),
          conditionalPanel(
            condition = paste0("input['", ns("coreParams"), "'] == true"),
            div(
              style = "margin: 4px 0 4px 24px;",
              div(
                style = "display:flex; flex-direction:column; gap:10px;",
                
                div(
                  class = "keys-card",
                  style = "width:100%;",
                  div(class = "keys-title", "Currency"),
                  selectInput(
                    ns("currency_sel"), NULL,
                    choices = c("EUR","USD","GBP","MXN"),
                    selected = "EUR"
                  )
                ),
                
                div(
                  class = "keys-card",
                  style = "width:100%;",
                  div(class = "keys-title", "Risk-weighting (%)"),
                  numericInput(
                    ns("risk_weighting_pct"), NULL, 75,
                    min = 0, max = 100, step = 0.1
                  )
                )
              )
            )
          ),
          
          # Risk parameters subgroup
          checkbox_custom("riskParams", "Risk parameters", FALSE),
          conditionalPanel(
            condition = paste0("input['", ns("riskParams"), "'] == true"),
            div(
              style = "margin: 4px 0 4px 24px;",
              div(
                class = "keys-card",
                style = "width:100%;",
                
                div(class = "keys-title", "Risk parameters"),
                
                div(style = "display:flex; flex-direction:column; gap:12px;",
                    numericInput(ns("base_pd"),  "Base Case PD (%)",  1.4,  min = 0, max = 100, step = 0.01),
                    numericInput(ns("base_lgd"), "Base Case LGD (%)", 50.0, min = 0, max = 100, step = 0.1),
                    numericInput(ns("base_w"),   "Base Case W (%)",   1.44, min = 0, max = 100, step = 0.01)
                )
              )
            )
          ),
          
          # Provisions & FX subgroup
          checkbox_custom("provFXParams", "Provisions/FX parameters", FALSE),
          conditionalPanel(
            condition = paste0("input['", ns("provFXParams"), "'] == true"),
            div(style = "margin: 4px 0 4px 24px;",
                div(class = "keys-card",
                    h6("Provisions / FX", class = "keys-title"),
                    fluidRow(
                      column(12, numericInput(ns("provisions"), "Provisions/SCRAs (%)", 1.22, min = 0, max = 100, step = 0.01))
                    ),
                    fluidRow(
                      column(12, numericInput(ns("fx_exp"),      "FX Exposure (%)",      0.00, min = 0, max = 100, step = 0.01))
                    ),
                    fluidRow(
                      column(12, numericInput(ns("fx_haircut"),  "FX Haircut (%)",       0.00, min = 0, max = 100, step = 0.01))
                    )
                )
            )
          )
        )
      )
    ),
    
    tags$hr(),
    keys_ref_portfolio_ui(ns),
    tags$hr(),
    
    
    ## 2) REGULATORY (main group)
    tags$hr(),
    h4("Regulatory"),
    div(
      style = "margin-left: 4px;",
      # start collapsed
      checkbox_custom("regulatoryBox", "Show regulatory inputs", FALSE),
      
      conditionalPanel(
        condition = paste0("input['", ns("regulatoryBox"), "'] == true"),
        div(style = "margin-left: 24px;",
            div(class = "keys-card",
                style = "padding:8px 10px;",
                fluidRow(
                  column(12, selectInput(ns("asset_rw_method"), "Asset RW Method", c("STD","IRB"), selected = "STD"))
                ),
                fluidRow(
                  column(12, selectInput(ns("sec_rw_method"),   "Securitisation RW Method", c("SEC-ERBA","SEC-SA","SEC-IRBA"), selected = "SEC-ERBA"))
                ),
                fluidRow(
                  column(12, checkboxInput(ns("sts"), "STS?", TRUE))
                ),
                fluidRow(
                  column(12, selectInput(ns("pool_type"), "Pool Type", c("Retail","Non-Retail"), selected = "Retail"))
                ),
                fluidRow(
                  column(12, checkboxInput(ns("funding_benefit"), "Funding Benefit?", FALSE))
                ),
                fluidRow(
                  column(12, radioButtons(ns("cost_of_capital"),
                                          label = "Cost of capital scope",
                                          choices = c("Local","Group"),
                                          selected = "Local",
                                          inline = FALSE))
                )
            )
        )
      )
    ),
    
    # =======================================================================
    # SEC-IRBA INPUTS (only show when SEC-IRBA is selected)
    # =======================================================================
    tags$hr(),
    conditionalPanel(
      condition = paste0("input['", ns("sec_rw_method"), "'] == 'SEC-IRBA'"),
      h4("SEC IRBA INPUTS"),
      div(
        style = "margin-left: 4px;",
        checkbox_custom("secIrbaInputsMain", "SEC IRBA inputs", FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("secIrbaInputsMain"), "'] == true"),
          div(
            class = "keys-card",
            style = "padding:8px 10px; margin-left:24px;",
            h6("SEC IRBA Inputs (editable)"),
            rhandsontable::rHandsontableOutput(ns("sec_irba_inputs_hot"), width = "100%", height = 220)
          )
        )
      )
    ),
    
    ## 3) STRUCTURE (main group)
    tags$hr(),
    h4("Structure"),
    div(
      style = "margin-left: 4px;",
      # collapsed by default
      checkbox_custom("structureMain", "Structure inputs", FALSE),
      conditionalPanel(
        condition = paste0("input['", ns("structureMain"), "'] == true"),
        div(style = "margin-left: 24px;",
            div(class = "keys-card", style = "padding:8px 10px;",
                
                checkbox_custom("dealInfo", "Deal info", FALSE),
                conditionalPanel(
                  condition = paste0("input['", ns("dealInfo"), "'] == true"),
                  div(style = "margin: 4px 0 4px 24px;",
                      selectInput(ns("deal_type"), "Type",
                                  choices = c("Traditional","Synthetic"), selected = "Traditional")
                  )
                ),
                
                checkbox_custom("datesBox", "Dates", FALSE),
                conditionalPanel(
                  condition = paste0("input['", ns("datesBox"), "'] == true"),
                  div(style = "margin: 4px 0 4px 24px;",
                      fluidRow(
                        column(12, dateInput(ns("effective_date"), "Effective Date",  value = "2023-11-01"))
                      ),
                      fluidRow(
                        column(12, dateInput(ns("final_maturity_date"), "Final Maturity Date", value = "2030-01-01"))
                      ),
                      fluidRow(
                        column(12, dateInput(ns("time_call_date"), "Time Call (if applicable)", value = "2028-01-01"))
                      )
                  )
                ),
                
                # After Time Call UI block, but before Amortisation Type UI
                fluidRow(
                  column(12,
                         div(
                           style = "padding: 2px 0 2px 0;",
                           tags$label("Clean-up Call (if applicable)", style="font-weight:500;"),
                           tags$div("10%", style="font-size:15px; color:#444; background:#f8fafc; border-radius:4px; padding:2px 10px; margin-bottom:2px;")
                         )
                  )
                ),
                fluidRow(
                  column(12,
                         div(
                           style = "padding: 2px 0 8px 0;",
                           tags$label("Revolving Period (Months)", style="font-weight:500;"),
                           tags$div("24", style="font-size:15px; color:#444; background:#f8fafc; border-radius:4px; padding:2px 10px; margin-bottom:2px;")
                         )
                  )
                ),
                
                checkbox_custom("mechanicsBox", "Mechanics", FALSE),
                conditionalPanel(
                  condition = paste0("input['", ns("mechanicsBox"), "'] == true"),
                  div(style = "margin: 4px 0 4px 24px;",
                      fluidRow(
                        column(12, selectInput(ns("amort_type"), "Amortisation Type", c("Sequential","Pro Rata"), selected = "Sequential"))
                      ),
                      fluidRow(
                        column(12, numericInput(ns("excess_spread"), "Excess Spread (Annual, %)", 0.00, min = 0, max = 100, step = 0.01))
                      ),
                      fluidRow(
                        column(12, selectInput(ns("frequency"), "Frequency", choices = c("A", "Q", "M"), selected = "M"))
                      )
                  )
                ),
                
                checkbox_custom("trancheInputs", "Tranche inputs (legacy toggle)", FALSE)
            )
        )
      )
    ),
    
    ## 4) COSTS (main group)
    tags$hr(),
    h4("Costs"),
    div(
      style = "margin-left: 4px;",
      checkbox_custom("costsBox", "Costs", FALSE),
      conditionalPanel(
        condition = paste0("input['", ns("costsBox"), "'] == true"),
        div(style = "margin-left: 24px;",
            div(class = "keys-card", style = "padding:8px 10px;",
                
                # Sub-checkbox: External Costs
                checkbox_custom("externalCostsBox", "External Costs", FALSE),
                conditionalPanel(
                  condition = paste0("input['", ns("externalCostsBox"), "'] == true"),
                  div(style = "margin: 4px 0 4px 24px;",
                      div(class = "keys-card",
                          div(style = "display:flex; flex-direction:column; gap:10px;",
                              div(style = "width:100%;",
                                  div(class = "keys-title", "Upfront"),
                                  numericInput(ns("external_upfront"), NULL, 1445000, min = 0, step = 1000)
                              ),
                              div(style = "width:100%;",
                                  div(class = "keys-title", "Running (Annual)"),
                                  numericInput(ns("external_running"), NULL, 100000, min = 0, step = 1000)
                              )
                          )
                      )
                  )
                ),
                
                # Sub-checkbox: Tax Rate
                checkbox_custom("taxRateBox", "Tax Rate", FALSE),
                conditionalPanel(
                  condition = paste0("input['", ns("taxRateBox"), "'] == true"),
                  div(style = "margin: 4px 0 4px 24px;",
                      div(class = "keys-card",
                          div(style = "display:flex; flex-direction:column; gap:10px;",
                              div(style = "width:100%;",
                                  div(class = "keys-title", "Tax Rate for Group (%)"),
                                  numericInput(ns("tax_group"), NULL, value = 28, min = 0, max = 100)
                              ),
                              div(style = "width:100%;",
                                  div(class = "keys-title", "Tax Rate for Local (%)"),
                                  numericInput(ns("tax_local"), NULL, value = 28, min = 0, max = 100)
                              )
                          ),
                          div(class="keys-note", "By default, Tax Rate for Local mirrors Group (as in the sheet)."),
                          checkboxInput(ns("keys_link_tax_local"), "Lock Local tax to Group", TRUE)
                      )
                  )
                )
            )
        )
      )
    ),
    
    ## 5) CAPITAL COMPONENTS (main group)
    tags$hr(),
    h4("Capital Components"),
    div(
      style = "margin-left: 4px;",
      checkbox_custom("capitalComponentsMain", "Capital components", FALSE),
      conditionalPanel(
        condition = paste0("input['", ns("capitalComponentsMain"), "'] == true"),
        div(id = ns("capitalChildren"),
            class = "keys-card",
            style = "padding:8px 10px; margin-left:24px;",
            h5("Capital tables"),
            tags$div(style = "height: 4px;"),
            
            conditionalPanel(
              condition = paste0("input['", ns("cost_of_capital"), "'] == 'Group'"),
              h6("Group (editable)"),
              rhandsontable::rHandsontableOutput(ns("cap_tbl_group_edit"), width = "100%", height = 160)
            ),
            conditionalPanel(
              condition = paste0("input['", ns("cost_of_capital"), "'] == 'Local'"),
              h6("Local (editable)"),
              rhandsontable::rHandsontableOutput(ns("cap_tbl_local_edit"), width = "100%", height = 160)
            ),
            
            tags$div(style = "height: 8px;"),
            tags$hr(),
            h5("Capital summary"),
            tableOutput(ns("cap_summary_table")),
            
            tags$div(style = "height: 8px;"),
            div(style = "display:flex; flex-direction:column; gap:10px;",
                div(style = "width:100%;",
                    div(class = "keys-title", "Overall Cost of Funding - Group (%)"),
                    numericInput(ns("ocf_group"), NULL, value = 1.47, min = 0, max = 100)
                ),
                div(style = "width:100%;",
                    div(class = "keys-title", "Overall Cost of Funding - Local (%)"),
                    numericInput(ns("ocf_local"), NULL, value = 1.47, min = 0, max = 100)
                )
            ),
            # In mod90_ui_input() inside the Capital Components main group
            # Put this at the very end of the capitalChildren div, AFTER the ocf_group/ocf_local inputs:
            
            tags$hr(),
            h5("Capital Cost Inputs"),
            tableOutput(ns("capital_cost_inputs_tbl"))
        )
      )
    ),
    
    ## 6) ADDITIONAL RETAINED SECURITISATION POSITIONS (main group)
    tags$hr(),
    h4("Additional Retained Securitisation Positions"),
    div(
      style = "margin-left: 4px;",
      checkbox_custom("retainedBox", "Additional Retained Securitisation Positions", FALSE),
      conditionalPanel(
        condition = paste0("input['", ns("retainedBox"), "'] == true"),
        div(style = "margin-left: 24px;",
            div(class = "keys-card", style = "padding:8px 10px;",
                helpText("Editable stub; final formulas will be wired from Keys sheet."),
                rhandsontable::rHandsontableOutput(ns("retained_positions_hot"), width = "100%", height = 140)
            )
        )
      )
    ),
    
    tags$hr()
    
  )
}


mod90_sidebar <- function(id, parent_tab_value = "SRT") {
  conditionalPanel(
    condition = sprintf("input.tabselected == '%s'", parent_tab_value),
    mod90_ui_input(id),
    style = "display: none;"
  )
}

mod90_ui_output <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns("SRT_subtab"),
      selected = "Outputs",   # CHANGED (was "Economics")
      
      # NEW FIRST TAB (Outputs)
      tabPanel(
        "Outputs", value = "Outputs",
        tags$div(
          h5("Capital Ratios", class = "table-heading"),
          tableOutput(ns("capital_ratios_tbl")),
          h5("EBA Summary", class = "table-heading"),
          tableOutput(ns("eba_summary_tbl")),
          
          
          uiOutput(ns("eba_tests_header_ui")),
          uiOutput(ns("test2_commensurate_ui")),
          uiOutput(ns("test1_min_thickness_ui")),
          uiOutput(ns("test3_comprehensive_ui"))
        )
      ),
      
      # EXISTING TABS (unchanged)
      tabPanel(
        "Economics", value = "Economics",
        tags$div(
          class = "econ-tables",   # NEW wrapper class - used by CSS + JS alignment
          
          h5("Economic Summary — Key Exposure and Risk Metrics", class = "table-heading"),
          tableOutput(ns("econ_summary_base_tbl")),
          tags$hr(),
          
          h5("Notional Amounts", class = "table-heading"),
          tableOutput(ns("econ_notional_base_tbl")),
          tags$hr(),
          
          h5("Risk Weights", class = "table-heading"),
          tableOutput(ns("econ_riskweights_base_tbl")),
          tags$hr(),
          
          h5("Risk-Weighted Exposure Amounts", class = "table-heading"),
          tableOutput(ns("econ_rwea_base_tbl")),
          tags$hr(),
          
          
          h5("Risk-Weighted Exposure by Category", class = "table-heading"),
          tableOutput(ns("econ_rw_exposure_base_tbl")),
          tags$hr(),
          h5("Non-Maximum Risk-Weighted Exposure Summary", class = "table-heading"),
          tableOutput(ns("econ_nonmax_rw_exposure_base_tbl")),
          tags$hr(),
          h5("Risk-Weighted Assets (RWEA) Reduction Analysis", class = "table-heading"),
          tableOutput(ns("econ_rwea_reduction_base_tbl")),
          tags$hr(),
          h5("CET1 Capital Impact and Provision Adjustments", class = "table-heading"),
          tableOutput(ns("econ_cet1_capital_effects_base_tbl")),
          tags$hr(),
          h5("Cost of Credit Protection — Gross Components", class = "table-heading"),
          tableOutput(ns("econ_cost_of_protection_base_tbl")),
          tags$hr(),
          h5("Cost of Credit Protection — Net Impact", class = "table-heading"),
          tableOutput(ns("econ_cost_of_protection_net_base_tbl")),
          tags$hr(),
          h5("Capital Cost Comparison", class = "table-heading"),
          tableOutput(ns("econ_comparison_capital_cost_base_tbl")),
          tags$hr(),
          h5("Deal Performance Metrics — Capital Efficiency Tests", class = "table-heading"),
          tableOutput(ns("econ_ccc_deal_tests_base_tbl")),
          tags$hr(),
          h5("Additional Deal Performance Indicators", class = "table-heading"),
          tableOutput(ns("econ_ccc_deal_tests_extra_labels_base_tbl")),
          tags$hr(),
          h5("Regulatory Expected and Unexpected Loss vs Non-Senior Tranches", class = "table-heading"),
          tableOutput(ns("econ_reg_elul_nonsenior_test_base_tbl")),
          tags$hr(),
          
          tags$details(
            tags$summary("🔧 Debug: bus & table payloads (mod90)"),
            tags$p("Top of upstream bus 'tranche_df':"),
            verbatimTextOutput(ns("dbg_tranche_df_head")),
            tags$p("Top of prepared tranche DF (computed A/D etc.):"),
            verbatimTextOutput(ns("dbg_tranche_df_prepared_head")),
            tags$p("Top of HOT DF (with Sold / Unfunded protection columns):"),
            verbatimTextOutput(ns("dbg_hot_df_head")),
            tags$p("Session-persistent inputs:"),
            verbatimTextOutput(ns("dbg_inputs_state"))
          )
        )
      ),
      tabPanel(
        "SEC-IRBA", value = "SEC-IRBA",
        conditionalPanel(
          condition = paste0("input['", ns("sec_rw_method"), "'] == 'SEC-IRBA'"),
          tags$div(
            h5("SEC-IRBA"),
            actionButton(ns("sec_irba_constants_btn"), "SEC-IRBA Constants"),
            tags$div(style = "height:10px;"),
            h6("SEC-IRBA Inputs"),
            tableOutput(ns("sec_irba_inputs_preview_tbl")),
            
            tags$hr(),
            h6("SEC-IRBA Inputs sheet"),
            tableOutput(ns("sec_irba_inputs_sheet_tbl"))
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("sec_rw_method"), "'] != 'SEC-IRBA'"),
          tags$div(
            style = "padding: 8px;",
            helpText("Select 'SEC-IRBA' as the Securitisation RW Method to view this tab.")
          )
        )
      ),
      tabPanel(
        "Amortisation", value = "Amortisation",
        tags$div(
          h5("Amortisation profile", class = "table-heading"),
          rhandsontable::rHandsontableOutput(ns("amort_table"), width = "100%", height = 400)
        )
      ),
      tabPanel(
        "Loss vectors", value = "Loss vectors",
        tags$div(
          h5("Loss vectors", class = "table-heading"),
          rhandsontable::rHandsontableOutput(ns("loss_table"), width = "100%", height = 400)
        )
      ),
      # NEW: ERBA viewer sub-tab (view-only)
      tabPanel(
        "ERBA", value = "ERBA",
        tags$div(
          style = "padding: 8px;",
          h5("SEC-ERBA viewer"),
          # Radio buttons to choose which ERBA block to view
          radioButtons(ns("erba_group"),
                       label = NULL,
                       choices = c(
                         "ERBA non-STC risk weights" = "nonstc",
                         "ERBA STC risk weights"     = "stc",
                         "Monthly data"             = "monthly"
                       ),
                       selected = "nonstc",
                       inline = FALSE),
          
          # Conditional panels show the relevant static table
          conditionalPanel(
            condition = paste0("input['", ns("erba_group"), "'] == 'nonstc'"),
            tags$div(style = "margin-top:8px;",
                     h5("ERBA non-STC risk weights", class = "table-heading"),
                     tableOutput(ns("erba_nonstc_tbl"))
            )
          ),
          conditionalPanel(
            condition = paste0("input['", ns("erba_group"), "'] == 'stc'"),
            tags$div(style = "margin-top:8px;",
                     h5("ERBA STC risk weights", class = "table-heading"),
                     tableOutput(ns("erba_stc_tbl"))
            )
          ),
          conditionalPanel(
            condition = paste0("input['", ns("erba_group"), "'] == 'monthly'"),
            tags$div(
              style = "margin-top:8px;",
              
              div(
                class = "erba-monthly-block",
                h5("Attachment points", class = "table-heading"),
                tableOutput(ns("erba_monthly_attach_tbl"))
              ),
              
              div(
                class = "erba-monthly-block",
                h5("Detachment points", class = "table-heading"),
                tableOutput(ns("erba_monthly_detach_tbl"))
              ),
              
              div(
                class = "erba-monthly-block",
                h5("T (Tranche thickness)", class = "table-heading"),
                tableOutput(ns("erba_monthly_T_tbl"))
              ),
              
              div(
                class = "erba-monthly-block",
                h5("WAL (0% CPR / 0% CDR)", class = "table-heading"),
                tableOutput(ns("erba_monthly_wal_tbl"))
              ),
              
              div(
                class = "erba-monthly-block",
                h5("MT (Maturity in years)", class = "table-heading"),
                tableOutput(ns("erba_monthly_mt_tbl"))
              ),
              
              div(
                class = "erba-monthly-block",
                h5("Rating", class = "table-heading"),
                tableOutput(ns("erba_monthly_rating_tbl"))
              ),
              
              div(
                class = "erba-monthly-block",
                h5("Senior 1-year ERBA risk weights", class = "table-heading"),
                tableOutput(ns("erba_monthly_snr1y_tbl"))
              ),
              
              div(
                class = "erba-monthly-block",
                h5("Senior 5-year ERBA risk weights", class = "table-heading"),
                tableOutput(ns("erba_monthly_snr5_tbl"))
              ),
              
              div(
                class = "erba-monthly-block",
                h5("Mezzanine 1-year ERBA risk weights", class = "table-heading"),
                tableOutput(ns("erba_monthly_mezz1_tbl"))
              ),
              
              div(
                class = "erba-monthly-block",
                h5("Mezzanine 5-year ERBA risk weights", class = "table-heading"),
                tableOutput(ns("erba_monthly_mezz5_tbl"))
              ),
              
              div(
                class = "erba-monthly-block",
                h5("RW Non-STS (Base)", class = "table-heading"),
                tableOutput(ns("erba_monthly_rw_nonsts_base_tbl"))
              )
            )
          )
        )
      )
    )
  )
}

mod90_tab <- function(id) {
  tabPanel(
    "SRT (mod90)", value = "SRT",
    div(style = "margin-top: 5px"),
    mod90_ui_output(id)
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# mod90_server — Core SRT Module
# -----------------------------------------------------------------------------

mod90_server <- function(id, bus) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    rv <- reactiveValues(
      sold = NULL,
      unfunded = NULL,
      base_periods = mosBase,
      tranche_names = NULL,
      max_tranches_from_scenario = NULL,
      currency = "EUR",
      risk_weight = 0.75,
      # NEW: store which SRT subtab (Economics / Amortisation / Loss vectors) was active
      last_SRT_subtab = "Economics"
    )
    
    # ---- Single source of truth: FIRST-LOAD DEFAULTS (mod90) ----
    .mod90_defaults <- list(
      # periods
      periods = function() if (!is.null(rv$base_periods)) rv$base_periods else mosBase,
      
      # Sidebar expand/collapse checkboxes
      checkboxes = list(
        portfolioInputs      = FALSE,
        coreParams           = FALSE,
        riskParams           = FALSE,
        provFXParams         = FALSE,
        regulatoryBox        = FALSE,
        structureMain        = FALSE,
        dealInfo             = FALSE,
        datesBox             = FALSE,
        mechanicsBox         = FALSE,
        trancheInputs        = FALSE,
        costsBox             = FALSE,
        externalCostsBox     = FALSE, 
        taxRateBox           = FALSE, 
        capitalComponentsMain= FALSE,
        retainedBox          = FALSE,
        keys_link_tax_local  = TRUE,
        secIrbaInputsMain    = FALSE
      ),
      
      # Portfolio inputs
      currency_sel         = "EUR",
      risk_weighting_pct   = 75,
      base_pd              = 1.4,
      base_lgd             = 50.0,
      base_w               = 1.44,
      provisions           = 1.22,
      fx_exp               = 0.00,
      fx_haircut           = 0.00,
      
      # Regulatory
      asset_rw_method      = "STD",
      sec_rw_method        = "SEC-ERBA",
      sts                  = TRUE,
      pool_type            = "Retail",
      funding_benefit      = FALSE,
      cost_of_capital      = "Local",
      
      # Structure
      deal_type            = "Traditional",
      effective_date       = as.Date("2023-11-01"),
      final_maturity_date  = as.Date("2030-01-01"),
      time_call_date       = as.Date("2028-01-01"),
      amort_type           = "Sequential",
      excess_spread        = 0.00,
      frequency            = "M",
      
      # Costs
      external_upfront     = 1445000,
      external_running     = 100000,
      tax_group            = 28,
      tax_local            = 28,
      
      # Capital components (yellow inputs)
      ocf_group            = 1.47,
      ocf_local            = 1.47,
      
      # Keys sheet inputs
      keys_ref_notional    = 600000000,
      keys_wal             = 2.78,
      keys_cpr             = 10,
      keys_n               = 0,
      keys_longest_mat_months = 60,
      
      # Subtab default
      SRT_subtab           = "Economics"
    )
    
    
    dcf <- reactive({
      freq <- input$frequency %||% "M"
      if (freq == "Q") {
        1/4
      } else if (freq == "M") {
        1/12
      } else {
        1
      }
    })
    
    keys_ref <- keys_bind_server(input, output, rv, ns, tranche_df_react = bus("tranche_df"))
    
    currency <- reactive(rv$currency)
    risk_w   <- reactive(rv$risk_weight)
    
    update_currency <- function(value) rv$currency <- value
    update_risk_w   <- function(value) rv$risk_weight <- value
    
    observe({
      if (isTRUE(input$keys_link_tax_local)) {
        val <- input$tax_group
        if (!is.null(val)) {
          shinyjs::disable(ns("tax_local"))
          try(updateNumericInput(session, "tax_local", value = val), silent = TRUE)
        }
      } else {
        shinyjs::enable(ns("tax_local"))
      }
    })
    
    # --- NEW: Make PD/LGD labels responsive to Asset RW Method (STD vs IRB) ---
    observeEvent(input$asset_rw_method, {
      method <- input$asset_rw_method %||% "STD"
      
      if (identical(method, "IRB")) {
        updateNumericInput(session, "base_pd",  label = "Reg PD (%)")
        updateNumericInput(session, "base_lgd", label = "Reg LGD (%)")
      } else {
        updateNumericInput(session, "base_pd",  label = "Base Case PD (%)")
        updateNumericInput(session, "base_lgd", label = "Base Case LGD (%)")
      }
    }, ignoreInit = FALSE)
    
    
    initialize_rv_values <- function(df) {
      n_rows <- nrow(df)
      sold <- rep(0, n_rows)
      if ("Tranche" %in% names(df)) {
        sold[grepl("^(Class|Tranche)\\s*C$", df$Tranche, ignore.case = TRUE)] <- 1
        sold[grepl("^(Class|Tranche)\\s*D$", df$Tranche, ignore.case = TRUE)] <- 1
      }
      list(
        sold     = sold,
        unfunded = rep(FALSE, n_rows)
      )
    }
    
    compute_tranche_DF_tbl <- function(df) {
      n   <- nrow(df)
      x   <- ifelse(is.na(df$Notional), 0, df$Notional)
      tot <- sum(x)
      
      if (n > 0) {
        if (tot > 0) {
          below  <- rev(cumsum(rev(x))) - x
          attach <- below / tot
          detach <- c(1, head(attach, -1))
        } else {
          attach <- rep(NA_real_, n)
          detach <- c(1, if (n > 1) rep(NA_real_, n - 1) else numeric(0))
        }
      } else {
        attach <- numeric(0); detach <- numeric(0)
      }
      
      df$Attach <- attach
      df$Detach <- detach
      df$`Capital structure` <- df$Detach - df$Attach
      
      base_cols      <- c("Tranche", "Notional")
      coupon         <- c("Coupon")
      computed_cols  <- c("Attach", "Detach", "Capital structure")
      wal_cols       <- c("WAL.scheduled", "WAL.unscheduled")[c("WAL.scheduled", "WAL.unscheduled") %in% names(df)]
      rating_col     <- if ("Rating" %in% names(df)) "Rating" else character(0)   # NEW
      
      df <- df[, c(base_cols, computed_cols, coupon, wal_cols, rating_col), drop = FALSE]
      df
    }
    
    
    pretty_headers <- function(cols) {
      if ("WAL.scheduled" %in% cols & "WAL.unscheduled" %in% cols) {
        gsub("WAL\\.scheduled", "WAL (scheduled)",
             gsub("WAL\\.unscheduled", "WAL (unscheduled)", cols))
      } else cols
    }
    
    build_hot <- function(df, col_headers) {
      ht <- rhandsontable::rhandsontable(
        df,
        rowHeaders = NULL,
        height = NA,
        colHeaders = col_headers
      ) |>
        rhandsontable::hot_table(
          stretchH = "none",
          contextMenu = FALSE,
          autoColumnSize = FALSE,
          wordWrap = TRUE,
          fixedColumnsLeft = 1
        ) |>
        rhandsontable::hot_cols(colWidths = 100) |>
        rhandsontable::hot_col("Tranche", readOnly = FALSE) |>
        rhandsontable::hot_col("Notional", format = "0,0.00", readOnly = TRUE) |>
        rhandsontable::hot_col("Attach", format = "0.00%", readOnly = TRUE) |>
        rhandsontable::hot_col("Detach", format = "0.00%", readOnly = TRUE) |>
        rhandsontable::hot_col("Capital structure", format = "0.00%", readOnly = TRUE) |>
        # Coupon is entered as percent points in the UI (e.g., 5.00 means 5%),
        # so display/edit it as a plain number, not as percent-format (which would show 500%).
        rhandsontable::hot_col("Coupon", format = "0.00", readOnly = FALSE, className = "ht-tranche-DF-input") |>
        rhandsontable::hot_col(which(names(df) == "WAL.scheduled"),   format = "0.00", readOnly = TRUE) |>
        rhandsontable::hot_col(which(names(df) == "WAL.unscheduled"), format = "0.00", readOnly = TRUE) |>
        rhandsontable::hot_col("Sold", format = "0.00%", className = "ht-tranche-DF-input") |>
        rhandsontable::hot_validate_numeric("Sold", min = 0, max = 1) |>
        rhandsontable::hot_col("Unfunded Protection?", type = "checkbox",
                               className = "ht-tranche-DF-input htCenter htMiddle")
      
      # NEW: editable Rating column, if present
      if ("Rating" %in% names(df)) {
        ht <- ht |> rhandsontable::hot_col("Rating", readOnly = FALSE)
      }
      
      ht
    }
    
    manage_tranche_modal <- function(action = c("open", "close")) {
      switch(
        action,
        "open" = {
          shinyjs::disable(ns("trancheInputs"))
          showModal(
            tags$div(
              class = "draggable-modal",
              modalDialog(
                title = "Tranche inputs",
                tagList(
                  tags$style(HTML("#shiny-modal .modal-header .close { display: none !important; }")),
                  h5("Tranches received via bus (from mod95):"),
                  uiOutput(ns("tranche_DF_hot_ui")),
                  div(style = "margin-top: 6px;"),
                  textOutput(ns("tranchedf_summary")),
                  textOutput(ns("tranchedf_cap_total"))
                ),
                easyClose = FALSE,
                footer = tagList(
                  actionButton(ns("trancheCloseBtn"), "Close")
                )
              )
            )
          )
        },
        "close" = {
          updateCheckboxInput(session, "trancheInputs", value = FALSE)
          shinyjs::enable(ns("trancheInputs"))
          removeModal()
        }
      )
    }
    
    render_debug_outputs <- function() {
      list(
        dbg_tranche_df_head = renderPrint({ df <- tranche_df(); req(df); utils::head(df) }),
        dbg_tranche_df_prepared_head = renderPrint({ df <- tranche_df_prepared(); req(df); utils::head(df) }),
        dbg_hot_df_head = renderPrint({ df <- hot_df(); req(df); utils::head(df) }),
        dbg_inputs_state = renderPrint({ mod90_inputs() })
      )
    }
    
    tranche_df <- bus("tranche_df")
    scenario_df <- bus("scenario_df")
    
    tranche_df_prepared <- reactive({
      df <- tranche_df(); req(df)
      compute_tranche_DF_tbl(df)
    })
    
    observeEvent(tranche_df(), {
      rv$tranche_names <- NULL
    }, ignoreInit = FALSE)
    
    # hot_df now derives Sold from upstream Sold.default if present, so that
    # edits in Mod95 editor stay in sync with the tranche inputs modal.
    hot_df <- reactive({
      df_raw <- tranche_df_prepared()
      df_up  <- tranche_df()
      
      # --- Sold: already bus-driven ---
      if (!is.null(df_up) && "Sold.default" %in% names(df_up) &&
          length(df_up$Sold.default) == nrow(df_raw)) {
        current_sold <- df_up$Sold.default
      } else {
        if (is.null(rv$sold) || length(rv$sold) != nrow(df_raw)) {
          initialized <- initialize_rv_values(df_raw)
          current_sold <- initialized$sold
        } else {
          current_sold <- rv$sold
        }
      }
      
      # --- NEW: Unfunded is also bus-driven (so legacy reflects edits made in mod95) ---
      if (!is.null(df_up) && "Unfunded.default" %in% names(df_up) &&
          length(df_up$Unfunded.default) == nrow(df_raw)) {
        current_unfunded <- as.logical(df_up$Unfunded.default)
      } else {
        if (is.null(rv$unfunded) || length(rv$unfunded) != nrow(df_raw)) {
          if (!exists("initialized") || is.null(initialized)) {
            initialized <- initialize_rv_values(df_raw)
          }
          current_unfunded <- initialized$unfunded
        } else {
          current_unfunded <- rv$unfunded
        }
      }
      
      rv$sold     <- current_sold
      rv$unfunded <- current_unfunded
      
      df <- df_raw
      df$Sold <- rv$sold
      df$`Unfunded Protection?` <- as.logical(rv$unfunded)
      
      # keep Sold + Unfunded at the end (existing behavior)
      df <- df[, c(setdiff(names(df), c("Sold", "Unfunded Protection?")), "Sold", "Unfunded Protection?")]
      df
    })
    
    
    observeEvent(tranche_df(), {
      df <- tranche_df(); req(df)
      summary_out <- data.frame(
        n_tranches = nrow(df),
        notional_total = sum(df$Notional),
        stringsAsFactors = FALSE
      )
      bus("mod90_summary", summary_out)
    }, ignoreInit = FALSE)
    
    Explain1010(input,"explain1010")
    
    # set initial value once on load (safe)
    observeEvent(input$periods, {
      # only set on load if base_periods somehow missing
      if (is.null(rv$base_periods) || !is.finite(rv$base_periods)) {
        rv$base_periods <- suppressWarnings(as.integer(input$periods))
        if (!is.finite(rv$base_periods)) rv$base_periods <- 60L
      }
    }, once = TRUE, ignoreInit = FALSE)
    
    
    # update periods when user clicks the checkmark
    observeEvent(input$periods_confirmed, {
      req(input$periods_confirmed)
      
      # robust parse (handles commas/spaces)
      p_chr <- as.character(input$periods_confirmed)
      p_int <- suppressWarnings(as.integer(gsub("[^0-9-]", "", p_chr)))
      
      req(is.finite(p_int), p_int >= 1L)
      rv$base_periods <- p_int
    }, ignoreInit = TRUE)
    
    
    observeEvent(input$currency_sel,  ignoreInit = TRUE, { update_currency(input$currency_sel) })
    observeEvent(input$risk_weighting_pct, ignoreInit = TRUE, {
      val <- suppressWarnings(as.numeric(input$risk_weighting_pct))
      if (!is.na(val)) update_risk_w(val/100)
    })
    
    core_df <- reactive({
      data.frame(
        Col1 = c(currency(), as.character(rv$risk_weight)),
        stringsAsFactors = FALSE,
        row.names = c("Currency", "Risk-weighting")
      )
    })
    
    output$coreParams_tbl <- rhandsontable::renderRHandsontable({
      req(FALSE)
      df <- core_df()
      rhandsontable::rhandsontable(df, stretchH = "none", height = 68) %>%
        rhandsontable::hot_col(1, width = 70) %>%
        rhandsontable::hot_table(
          manualColumnResize = FALSE,
          contextMenu = FALSE,
          cells = htmlwidgets::JS("getCellConfig")
        ) %>%
        htmlwidgets::onRender(htmlwidgets::JS("initTable"))
    })
    
    observeEvent(input$coreParams_tbl, {
      tbl <- rhandsontable::hot_to_r(input$coreParams_tbl)
      if (is.null(tbl)) return()
      update_currency(tbl[1, 1])
      val <- suppressWarnings(as.numeric(tbl[2, 1]))
      if (!is.na(val)) update_risk_w(val)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    cap_group <- reactiveVal(.cap_defaults_group)
    cap_local <- reactiveVal(.cap_defaults_local)
    
    output$cap_tbl_group_preview <- renderTable({
      g <- cap_group()
      data.frame(
        Component = c("CET1","AT1","T2","SNP"),
        `Distribution (% RWAs)` = round(100 * unname(g$distribution), 4),
        `Cost of Capital (%)`   = round(100 * unname(g$cost), 4),
        check.names = FALSE
      )
    }, striped = TRUE, bordered = TRUE, digits = 4)
    
    output$cap_tbl_local_preview <- renderTable({
      l <- cap_local()
      data.frame(
        Component = c("CET1","AT1","T2","SNP"),
        `Distribution (% RWAs)` = round(100 * unname(l$distribution), 4),
        `Cost of Capital (%)`   = round(100 * unname(l$cost), 4),
        check.names = FALSE
      )
    }, striped = TRUE, bordered = TRUE, digits = 4)
    
    cap_df_to_hot <- function(x) {
      data.frame(
        Component = c("CET1","AT1","T2","SNP"),
        `Distribution (% RWAs)` = round(100 * unname(x$distribution), 4),
        `Cost of Capital (%)`   = round(100 * unname(x$cost), 4),
        check.names = FALSE
      )
    }
    
    capital_components_tbl <- reactive({
      scope <- input$cost_of_capital %||% "Local"
      if (identical(scope, "Group")) {
        cap_df_to_hot(cap_group())
      } else {
        cap_df_to_hot(cap_local())
      }
    })
    
    
    output$cap_tbl_group_edit <- rhandsontable::renderRHandsontable({
      req(input$capitalComponentsMain, input$cost_of_capital == "Group")
      df <- cap_df_to_hot(cap_group())
      rhandsontable::rhandsontable(df, stretchH = "none", height = 160) |>
        rhandsontable::hot_col("Distribution (% RWAs)", format = "0.0000") |>
        rhandsontable::hot_col("Cost of Capital (%)",   format = "0.0000") |>
        rhandsontable::hot_validate_numeric("Distribution (% RWAs)", min = 0, max = 100) |>
        rhandsontable::hot_validate_numeric("Cost of Capital (%)",   min = 0, max = 100)
    })
    
    observeEvent(input$cap_tbl_group_edit, {
      req(input$cost_of_capital == "Group")
      tbl <- rhandsontable::hot_to_r(input$cap_tbl_group_edit)
      if (is.null(tbl)) return()
      cap_group(list(
        distribution = as.numeric(tbl$`Distribution (% RWAs)`) / 100,
        cost         = as.numeric(tbl$`Cost of Capital (%)`)   / 100
      ))
    }, ignoreInit = TRUE)
    
    output$cap_tbl_local_edit <- rhandsontable::renderRHandsontable({
      req(input$capitalComponentsMain, input$cost_of_capital == "Local")
      df <- cap_df_to_hot(cap_local())
      rhandsontable::rhandsontable(df, stretchH = "none", height = 160) |>
        rhandsontable::hot_col("Distribution (% RWAs)", format = "0.0000") |>
        rhandsontable::hot_col("Cost of Capital (%)",   format = "0.0000") |>
        rhandsontable::hot_validate_numeric("Distribution (% RWAs)", min = 0, max = 100) |>
        rhandsontable::hot_validate_numeric("Cost of Capital (%)",   min = 0, max = 100)
    })
    
    observeEvent(input$cap_tbl_local_edit, {
      req(input$cost_of_capital == "Local")
      tbl <- rhandsontable::hot_to_r(input$cap_tbl_local_edit)
      if (is.null(tbl)) return()
      cap_local(list(
        distribution = as.numeric(tbl$`Distribution (% RWAs)`) / 100,
        cost         = as.numeric(tbl$`Cost of Capital (%)`)   / 100
      ))
    }, ignoreInit = TRUE)
    
    retained_defaults <- data.frame(
      Name    = c("Position #1", "Position #2", "Position #3"),
      Type    = c("Traditional", "Traditional", "Traditional"),
      Notional= c(50000, 20000, 50000),
      EAD     = c(10000, 10000, 10000),
      `RW%`   = c(100, 100, 100),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    rv$retained_positions <- retained_defaults
    
    output$retained_positions_hot <- rhandsontable::renderRHandsontable({
      req(input$retainedBox)
      rhandsontable::rhandsontable(rv$retained_positions, stretchH = "none", height = 140) |>
        rhandsontable::hot_col("Notional", format = "0,0") |>
        rhandsontable::hot_col("EAD",      format = "0,0") |>
        rhandsontable::hot_col("RW%",      format = "0.00") |>
        rhandsontable::hot_validate_numeric("RW%", min = 0, max = 100)
    })
    
    observeEvent(input$retained_positions_hot, {
      tbl <- rhandsontable::hot_to_r(input$retained_positions_hot)
      if (is.data.frame(tbl)) rv$retained_positions <- tbl
    }, ignoreInit = TRUE)
    
    
    # ------------------------------------------------------------------
    # SEC-IRBA Inputs (only relevant when SEC-IRBA is selected)
    # ------------------------------------------------------------------
    sec_irba_inputs_rv <- reactiveVal(sec_irba_default_inputs_df())
    
    output$sec_irba_inputs_hot <- rhandsontable::renderRHandsontable({
      req(input$sec_rw_method == "SEC-IRBA")
      df <- sec_irba_inputs_rv()
      
      rhandsontable::rhandsontable(df, stretchH = "none", height = 220) |>
        rhandsontable::hot_col("Item", readOnly = TRUE) |>
        rhandsontable::hot_col("A", format = "0.00") |>
        rhandsontable::hot_col("B", format = "0.00") |>
        rhandsontable::hot_col("C", format = "0.00") |>
        rhandsontable::hot_col("D", format = "0.00") |>
        rhandsontable::hot_col("E", format = "0.00") |>
        rhandsontable::hot_validate_numeric("A") |>
        rhandsontable::hot_validate_numeric("B") |>
        rhandsontable::hot_validate_numeric("C") |>
        rhandsontable::hot_validate_numeric("D") |>
        rhandsontable::hot_validate_numeric("E")
    })
    
    observeEvent(input$sec_irba_inputs_hot, {
      req(input$sec_rw_method == "SEC-IRBA")
      tbl <- rhandsontable::hot_to_r(input$sec_irba_inputs_hot)
      if (!is.data.frame(tbl)) return()
      
      # Keep Item column fixed and coerce numeric columns
      tbl$Item <- sec_irba_default_inputs_df()$Item
      for (nm in c("A","B","C","D","E")) {
        tbl[[nm]] <- suppressWarnings(as.numeric(tbl[[nm]]))
      }
      
      sec_irba_inputs_rv(tbl)
    }, ignoreInit = TRUE)
    
    output$sec_irba_inputs_preview_tbl <- renderTable({
      req(input$sec_rw_method == "SEC-IRBA")
      df <- sec_irba_inputs_rv()
      df
    }, rownames = FALSE)
    
    observeEvent(input$sec_irba_constants_btn, {
      req(input$sec_rw_method == "SEC-IRBA")
      
      showModal(
        modalDialog(
          title = "SEC-IRBA Constants",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          tableOutput(ns("sec_irba_constants_tbl"))
        )
      )
    })
    
    output$sec_irba_constants_tbl <- renderTable({
      req(input$sec_rw_method == "SEC-IRBA")
      sec_irba_constants_df()
    }, rownames = FALSE)
    
    # ------------------------------------------------------------------
    # SEC-IRBA: "Inputs sheet" computed table (headers + Floor/N + labels)
    # ------------------------------------------------------------------
    
    sec_irba_inputs_sheet_tbl <- reactive({
      req(input$sec_rw_method == "SEC-IRBA")
      
      # tranche names: use edited tranche names if present, else upstream tranche_df labels
      df_tr0 <- tranche_df(); req(df_tr0)
      
      tranche_names <- if (!is.null(rv$tranche_names) && length(rv$tranche_names)) {
        rv$tranche_names
      } else {
        as.character(df_tr0$Tranche)
      }
      
      # Build tranche keys table (already computed elsewhere; reuse to ensure parity)
      tk <- tranche_keys_base(); req(tk)
      
      # Key!C23: STS? checkbox => "Yes"/"No" in Excel, boolean here
      sts_flag <- isTRUE(input$sts)
      
      # Key!C15: N input in Reference Portfolio Inputs => keys_n
      n_val <- suppressWarnings(as.numeric(input$keys_n))
      if (!is.finite(n_val)) n_val <- 0
      
      sec_irba_build_inputs_sheet_table(
        tranche_names   = tranche_names,
        tranche_keys_df = tk,
        sts_flag        = sts_flag,
        n_value         = n_val,
        pool_type_value = input$pool_type %||% "",
        constants_df    = sec_irba_constants_df(),
        
        # Portfolio ▸ Risk parameters
        base_pd_pct     = input$base_pd  %||% 0,
        base_lgd_pct    = input$base_lgd %||% 0,
        
        # Portfolio ▸ Core parameters (RW %)
        rw_pct          = input$risk_weighting_pct %||% 0
      )
    })
    
    output$sec_irba_inputs_sheet_tbl <- renderTable({
      df <- sec_irba_inputs_sheet_tbl(); req(df)
      
      # Format Floor row as percent with 1 decimal (10.0%, 15.0%) like screenshot
      # Keep other values as-is for now.
      label_col <- names(df)[1]
      tranche_cols <- setdiff(names(df), label_col)
      
      # Find "Floor (%)" row and format only those cells
      idx_floor <- which(as.character(df[[label_col]]) == "Floor (%)")
      if (length(idx_floor) == 1) {
        for (cn in tranche_cols) {
          v <- suppressWarnings(as.numeric(df[idx_floor, cn, drop = TRUE]))
          if (is.finite(v)) df[idx_floor, cn] <- sprintf("%.1f%%", 100 * v)
        }
      }
      
      df
    }, rownames = FALSE, sanitize.text.function = function(x) x)
    
    
    # ------------------------------------------------------------------
    # SEC-IRBA: Computed "Inputs sheet" header rows (NEW)
    # ------------------------------------------------------------------
    
    sec_irba_sheet_header_tbl <- reactive({
      req(input$sec_rw_method == "SEC-IRBA")
      
      # Tranche names must come from "Tranche inputs (legacy toggle)" tranche column.
      # In this module, tranche names are stored either:
      #  - rv$tranche_names (if user edited names in the tranche modal)
      #  - tranche_df_prepared()$Tranche (default)
      df_tr <- tranche_df_prepared(); req(df_tr)
      
      tranche_names <- if (!is.null(rv$tranche_names) && length(rv$tranche_names) >= 1) {
        rv$tranche_names
      } else {
        as.character(df_tr$Tranche)
      }
      
      # "Key!$E$6:$Q$11" analogue: tranche_keys_base()
      tk <- tranche_keys_base(); req(tk)
      
      sec_irba_build_header_table(
        tranche_names   = tranche_names,
        tranche_keys_df = tk
      )
    })
    
    output$sec_irba_inputs_sheet_hdr_tbl <- renderTable({
      sec_irba_sheet_header_tbl()
    }, rownames = FALSE, sanitize.text.function = function(x) x)
    
    
    #--------------------------------------
    observeEvent(input$open_tranche_modal, {
      updateCheckboxInput(session, "trancheInputs", value = TRUE)
    }, ignoreInit = TRUE)
    
    output$tranche_preview <- renderTable({
      df <- tranche_df_prepared(); req(df)
      head(df[, c("Tranche","Notional","Attach","Detach","Capital structure","Coupon")], 6)
    }, striped = TRUE, bordered = TRUE, digits = 4)
    
    output$demo_outputs <- renderTable({
      data.frame(
        Item  = c("Periods", "Periods x 2", "Currency", "Risk-weighting", "RW x 2"),
        Value = c(
          rv$base_periods,
          rv$base_periods * 2,
          rv$currency,
          sprintf("%.2f%%", 100 * rv$risk_weight),
          rv$risk_weight * 2
        ),
        check.names = FALSE
      )
    }, rownames = FALSE)
    
    
    output$econ_summary_base_tbl <- render_pro_table({
      df <- econ_summary_base(); req(df)
      df <- econ_with_period0_headers(df)
      
      # ---- Formatting only (do not change underlying econ_summary_base()) ----
      period_cols <- setdiff(names(df), c("Label"))  # includes period cols + Aggregate
      
      fmt_num0_commas <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        v[!is.finite(v)] <- NA_real_
        ifelse(is.na(v), "-", format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE))
      }
      
      fmt_pct0 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        v[!is.finite(v)] <- NA_real_
        ifelse(is.na(v), "-", sprintf("%.0f%%", round(100 * v, 0)))
      }
      
      for (pc in period_cols) {
        # Keep CALLED / "" as-is for the call rows (but convert "" -> "-")
        is_call_row <- df$Label %in% c("Clean-up Call (if applicable)", "Time Call (if applicable)")
        call_vals <- as.character(df[[pc]])
        call_vals[!nzchar(trimws(call_vals))] <- "-"
        df[[pc]][is_call_row] <- call_vals[is_call_row]
        
        # WA RW (%) row: show as percent (0.75 -> 75%)
        is_warw <- df$Label == "WA RW (%)"
        df[[pc]][is_warw] <- fmt_pct0(df[[pc]][is_warw])
        
        # Numeric amount rows: commas + blanks as "-"
        is_amount <- df$Label %in% c(
          "Notional of Securitised Exposures",
          "Provisions(STD)",
          "RWEAs of Securitised Exposures"
        )
        df[[pc]][is_amount] <- fmt_num0_commas(df[[pc]][is_amount])
      }
      
      df <- apply_pro_table_ui_keep_position(df)
      df
    })
    
    
    output$econ_notional_base_tbl <- render_pro_table({
      df <- econ_notional_base(); req(df)
      df <- econ_with_period0_headers(df)
      
      # ---- Formatting only (do not change underlying econ_notional_base()) ----
      period_cols <- setdiff(names(df), c("Position", "Label"))
      
      fmt_num0_commas <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        v[!is.finite(v)] <- NA_real_
        ifelse(is.na(v), "-", format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE))
      }
      
      for (pc in period_cols) {
        df[[pc]] <- fmt_num0_commas(df[[pc]])
      }
      
      # Also show missing Position values as "-" (instead of "NA")
      if ("Position" %in% names(df)) {
        pos <- as.character(df$Position)
        pos[!nzchar(trimws(pos)) | is.na(pos)] <- "-"
        df$Position <- pos
      }
      
      df <- apply_pro_table_ui_keep_position(df)
      df
    })
    
    output$econ_riskweights_base_tbl <- render_pro_table({
      df <- econ_riskweights_base(); req(df)
      
      df_fmt <- econ_with_period0_headers(df)
      
      # numeric columns are all period columns (everything except Position / Label)
      num_cols <- setdiff(names(df_fmt), c("Position", "Label"))
      
      for (nm in num_cols) {
        v <- df_fmt[[nm]]
        df_fmt[[nm]] <- ifelse(
          is.na(v),
          "-",                # keep NA as NA
          sprintf("%.1f%%", round(as.numeric(v), 1))   # one decimal + % sign
        )
      }
      
      df_fmt <- apply_pro_table_ui_keep_position(df_fmt)
      df_fmt
    })
    
    output$econ_rwea_base_tbl <- render_pro_table({
      df <- econ_rwea_base(); req(df)
      df <- econ_with_period0_headers(df)
      
      period_cols <- setdiff(names(df), c("Position", "Label"))
      
      df[period_cols] <- lapply(df[period_cols], function(x) {
        v <- suppressWarnings(as.numeric(x))
        v[!is.finite(v)] <- NA_real_
        ifelse(is.na(v), "", format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE))
      })
      df <- apply_pro_table_ui_keep_position(df)
      df
    })
    
    output$econ_rw_exposure_base_tbl <- render_pro_table({
      df <- econ_rw_exposure_base(); req(df)
      df <- econ_with_period0_headers(df)
      
      # Add comma formatting for numeric columns (periods + Aggregate)
      num_cols <- setdiff(names(df), c("Position", "Label"))
      df[num_cols] <- lapply(df[num_cols], function(x) {
        v <- suppressWarnings(as.numeric(x))
        v[!is.finite(v)] <- NA_real_
        ifelse(is.na(v), "", format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE))
      })
      df$Position <- NULL
      df <- apply_pro_table_ui_keep_position(df)
      df
    })
    
    
    output$econ_nonmax_rw_exposure_base_tbl <- render_pro_table({
      df <- econ_exposure_summary_base(); req(df)
      
      # Optional: apply same period-0 header formatting like your other tables
      df <- econ_with_period0_headers(df)
      
      period_cols <- setdiff(names(df), c("Position", "Label"))
      
      df[period_cols] <- lapply(df[period_cols], function(x) {
        v <- suppressWarnings(as.numeric(x))
        v[!is.finite(v)] <- NA_real_
        ifelse(is.na(v), "", format(as.numeric(v), big.mark = ",", scientific = FALSE, trim = TRUE))
      })
      df$Position <- NULL
      df <- apply_pro_table_ui_keep_position(df)
      df
    })
    
    
    output$econ_rwea_reduction_base_tbl <- render_pro_table({
      df <- econ_rwea_reduction_base(); req(df)
      
      df <- econ_with_period0_headers(df)
      
      # Format:
      # - Row 1 (amount): comma, 0 decimals
      # - Row 2 and Row 4 (% rows): percent
      # - Row 3 (equivalent amount-like ratio): keep decimals (Excel-like), but you can adjust
      period_cols <- setdiff(names(df), c("Position", "Label"))
      
      # Identify rows by Position
      pos <- as.character(df$Position)
      
      for (pc in period_cols) {
        colv <- suppressWarnings(as.numeric(df[[pc]]))
        
        df[[pc]] <- ifelse(
          pos %in% c("RWEAs - Reduction", "Equivalent RWEAs - Reduction"),
          ifelse(is.na(colv), "", format(round(colv, 0), big.mark = ",", scientific = FALSE, trim = TRUE)),
          ifelse(
            pos %in% c("RWEAs - Reduction (%)", "Equivalent RWEAs - Reduction (%)"),
            ifelse(is.na(colv), "", sprintf("%.1f%%", round(100 * colv, 1))),
            ifelse(is.na(colv), "", as.character(colv))
          )
        )
      }
      df$Position <- NULL
      df <- apply_pro_table_ui_keep_position(df)
      df
    })
    
    
    output$econ_cet1_capital_effects_base_tbl <- render_pro_table({
      df <- econ_cet1_capital_effects_base(); req(df)
      
      # Relabel internal period headers to 0..N-1 for UI
      df <- econ_with_period0_headers(df)
      
      # Format numeric cells (keep blanks as "", show 0dp with commas)
      period_cols <- setdiff(names(df), "Label")
      df[period_cols] <- lapply(df[period_cols], function(x) {
        v <- suppressWarnings(as.numeric(x))
        v[!is.finite(v)] <- NA_real_
        ifelse(is.na(v), "", format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE))
      })
      df <- apply_pro_table_ui_keep_position(df)
      df
    })
    
    output$econ_cost_of_protection_base_tbl <- render_pro_table({
      df <- econ_cost_of_protection_base(); req(df)
      
      # Relabel internal period headers to 0..N-1 for UI (same pattern)
      df <- econ_with_period0_headers(df)
      
      # Format numeric cells: 0dp, commas; keep blanks ""
      period_cols <- setdiff(names(df), "Label")
      df[period_cols] <- lapply(df[period_cols], function(x) {
        v <- suppressWarnings(as.numeric(x))
        v[!is.finite(v)] <- NA_real_
        ifelse(is.na(v), "", format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE))
      })
      df <- apply_pro_table_ui_keep_position(df)
      df
    })
    
    
    # Track SRT subtab for save/restore of tab state
    observeEvent(input$SRT_subtab, {
      rv$last_SRT_subtab <- input$SRT_subtab
    }, ignoreInit = FALSE)
    
    
    output$econ_cost_of_protection_net_base_tbl <- render_pro_table({
      df <- econ_cost_of_protection_net_base(); req(df)
      
      # Relabel headers to 0..N-1 for UI
      df <- econ_with_period0_headers(df)
      
      # Format numeric cells: 0dp, commas; keep blanks ""
      period_cols <- setdiff(names(df), "Label")
      df[period_cols] <- lapply(df[period_cols], function(x) {
        v <- suppressWarnings(as.numeric(x))
        v[!is.finite(v)] <- NA_real_
        ifelse(is.na(v), "", format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE))
      })
      df <- apply_pro_table_ui_keep_position(df)
      df
    })
    
    
    output$econ_comparison_capital_cost_base_tbl <- render_pro_table({
      df <- econ_comparison_capital_cost_base(); req(df)
      
      # Already labeled 0..N-1 inside EconCore function output.
      # Format numeric cells like other amount tables: 0dp + commas; keep blanks as "".
      period_cols <- setdiff(names(df), "Label")
      
      df[period_cols] <- lapply(df[period_cols], function(x) {
        v <- suppressWarnings(as.numeric(x))
        ifelse(
          is.na(v),
          as.character(x),  # preserves "" in Total row periods
          format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
        )
      })
      df <- apply_pro_table_ui_keep_position(df)
      df
    })
    
    
    output$econ_ccc_deal_tests_base_tbl <- render_pro_table({
      df <- econ_ccc_deal_tests_base(); req(df)
      df <- econ_with_period0_headers(df)
      
      period_cols <- setdiff(names(df), c("Label", "Aggregate"))
      
      # First two rows are % rows (fractions). Format like Excel: 1 decimal percent.
      pct_rows <- df$Label %in% c(
        "Equivalent RWEAs - Reduction (%) - Closing",
        "Equivalent RWEAs - Reduction (%) - Lifetime"
      )
      
      for (pc in period_cols) {
        v_num <- suppressWarnings(as.numeric(df[[pc]]))
        
        df[[pc]] <- ifelse(
          pct_rows,
          ifelse(
            is.na(v_num) | df[[pc]] == "",
            "",
            sprintf("%.1f%%", round(v_num * 100, 1))
          ),
          df[[pc]]  # leave other rows (EV / Pass-Fail / CoC) untouched
        )
      }
      df <- apply_pro_table_ui_keep_position(df)
      df
    })
    
    
    # -------------------------
    # MASTER SNAPSHOT FOR SAVE
    # -------------------------
    # UPDATED: include all mod90-owned inputs + keys + retained + tab state
    # and full tranche_state (for restoring mod95 tranche edits)
    mod90_inputs <- reactive({
      # periods handling unchanged
      periods_val <- if (!is.null(rv$base_periods)) rv$base_periods else mosBase
      
      
      # Safely snapshot current tranche_df from bus for full tranche_state
      tranche_snapshot <- tryCatch({
        df <- tranche_df()
        if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) df else NULL
      }, error = function(e) NULL)
      
      # Ensure tranche_modal vectors line up with snapshot rows if available
      n_tr <- if (!is.null(tranche_snapshot)) nrow(tranche_snapshot) else length(rv$sold %||% numeric(0))
      sold_vec <- if (!is.null(rv$sold)) rv$sold else numeric(0)
      if (length(sold_vec) < n_tr) sold_vec <- c(sold_vec, rep(0, n_tr - length(sold_vec)))
      if (length(sold_vec) > n_tr) sold_vec <- sold_vec[seq_len(n_tr)]
      
      unfunded_vec <- if (!is.null(rv$unfunded)) as.logical(rv$unfunded) else logical(0)
      if (length(unfunded_vec) < n_tr) unfunded_vec <- c(unfunded_vec, rep(FALSE, n_tr - length(unfunded_vec)))
      if (length(unfunded_vec) > n_tr) unfunded_vec <- unfunded_vec[seq_len(n_tr)]
      
      # NEW: snapshot SEC-IRBA inputs table (so users don't lose edits)
      sec_irba_tbl_snapshot <- NULL
      if (exists("sec_irba_inputs_rv") && is.function(sec_irba_inputs_rv)) {
        sec_irba_tbl_snapshot <- tryCatch(sec_irba_inputs_rv(), error = function(e) NULL)
      }
      
      list(
        periods = periods_val,
        
        # Panel checkbox/open/close state
        checkboxes = list(
          portfolioInputs      = isTRUE(input$portfolioInputs),
          coreParams           = isTRUE(input$coreParams),
          riskParams           = isTRUE(input$riskParams),
          provFXParams         = isTRUE(input$provFXParams),
          capitalInputs        = isTRUE(input$capitalComponentsMain),
          trancheInputs        = isTRUE(input$trancheInputs),
          regulatoryBox        = isTRUE(input$regulatoryBox),
          structureMain        = isTRUE(input$structureMain),
          dealInfo             = isTRUE(input$dealInfo),
          datesBox             = isTRUE(input$datesBox),
          mechanicsBox         = isTRUE(input$mechanicsBox),
          costsBox             = isTRUE(input$costsBox),
          externalCostsBox     = isTRUE(input$externalCostsBox),
          taxRateBox           = isTRUE(input$taxRateBox), 
          retainedBox          = isTRUE(input$retainedBox),
          keys_link_tax_local  = isTRUE(input$keys_link_tax_local),
          
          # NEW: SEC-IRBA subgroup toggle (only visible if sec_rw_method == "SEC-IRBA")
          secIrbaInputsMain    = isTRUE(input$secIrbaInputsMain)
        ),
        
        # Portfolio ▸ Core
        core_params = list(
          currency    = rv$currency,
          risk_weight = rv$risk_weight,
          currency_sel = input$currency_sel,
          risk_weighting_pct = input$risk_weighting_pct
        ),
        
        # Portfolio ▸ Risk
        risk_params = list(
          base_pd  = input$base_pd,
          base_lgd = input$base_lgd,
          base_w   = input$base_w
        ),
        
        # Portfolio ▸ Provisions / FX
        provfx_params = list(
          provisions = input$provisions,
          fx_exp     = input$fx_exp,
          fx_haircut = input$fx_haircut
        ),
        
        # Regulatory
        regulatory = list(
          asset_rw_method  = input$asset_rw_method,
          sec_rw_method    = input$sec_rw_method,
          sts              = isTRUE(input$sts),
          pool_type        = input$pool_type,
          funding_benefit  = isTRUE(input$funding_benefit),
          cost_of_capital  = input$cost_of_capital,
          regulatoryBox    = isTRUE(input$regulatoryBox)
        ),
        
        # Structure (including subgroups + dates)
        structure = list(
          structureMain        = isTRUE(input$structureMain),
          deal_type            = input$deal_type,
          dealInfo             = isTRUE(input$dealInfo),
          datesBox             = isTRUE(input$datesBox),
          effective_date       = input$effective_date,
          final_maturity_date  = input$final_maturity_date,
          time_call_date       = input$time_call_date,
          mechanicsBox         = isTRUE(input$mechanicsBox),
          amort_type           = input$amort_type,
          excess_spread        = input$excess_spread,
          
          # NEW: Frequency (A/Q/M) affects dcf() and downstream tables
          frequency            = input$frequency
        ),
        
        # Costs & tax panel
        costs_tax = list(
          costsBox         = isTRUE(input$costsBox),
          external_upfront = input$external_upfront,
          external_running = input$external_running,
          tax_group        = input$tax_group,
          tax_local        = input$tax_local,
          keys_link_tax_local = isTRUE(input$keys_link_tax_local)
        ),
        
        # Yellow capital inputs: ocf + editable tables
        capital_yellow = list(
          capitalComponentsMain = isTRUE(input$capitalComponentsMain),
          ocf_group = input$ocf_group,
          ocf_local = input$ocf_local,
          group_table = cap_df_to_hot(cap_group()),
          local_table = cap_df_to_hot(cap_local())
        ),
        
        # Tranche modal state (yellow columns + names)
        tranche_modal = list(
          sold                = sold_vec,
          unfunded_protection = unfunded_vec,
          tranche_names       = if (!is.null(rv$tranche_names)) rv$tranche_names else character(0)
        ),
        
        # NEW: Full tranche state snapshot so mod95 edits are captured
        tranche_state = list(
          tranche_df    = tranche_snapshot,
          tranche_names = if (!is.null(rv$tranche_names)) rv$tranche_names else character(0)
        ),
        
        # Retained securitisation positions
        retained_positions = list(
          retainedBox      = isTRUE(input$retainedBox),
          table            = rv$retained_positions
        ),
        
        # Keys sheet inputs (those owned by mod90 UI)
        keys_inputs = list(
          keys_ref_notional       = input$keys_ref_notional,
          keys_wal                = input$keys_wal,
          keys_cpr                = input$keys_cpr,
          keys_n                  = input$keys_n,
          keys_longest_mat_months = input$keys_longest_mat_months
        ),
        
        # NEW: SEC-IRBA editable table snapshot
        sec_irba_inputs = list(
          table = sec_irba_tbl_snapshot
        ),
        
        # SRT subtab state (Economics / Amortisation / Loss vectors)
        subtab_state = list(
          SRT_subtab = rv$last_SRT_subtab %||% "Economics"
        )
      )
    })
    
    all_inputs <- reactive({
      list(
        currency = rv$currency,
        rw       = rv$risk_weight,
        base_pd  = from_pct_num(input$base_pd),
        base_lgd = from_pct_num(input$base_lgd),
        base_w   = from_pct_num(input$base_w),
        provisions = from_pct_num(input$provisions),
        fx_exp     = from_pct_num(input$fx_exp),
        fx_haircut = from_pct_num(input$fx_haircut),
        asset_rw_method = input$asset_rw_method %||% "STD",
        sec_rw_method   = input$sec_rw_method %||% "SEC-ERBA",
        sts             = isTRUE(input$sts),
        pool_type       = input$pool_type %||% "Retail",
        funding_benefit = isTRUE(input$funding_benefit),
        cost_of_capital = input$cost_of_capital %||% "Local",
        amort_type    = input$amort_type %||% "Sequential",
        excess_spread = from_pct_num(input$excess_spread),
        dcf = dcf(),
        external_upfront = input$external_upfront %||% 0,
        external_running = input$external_running %||% 0,
        tax_group  = from_pct_num(input$tax_group),
        tax_local  = from_pct_num(input$tax_local),
        ocf_group = from_pct_num(input$ocf_group),
        ocf_local = from_pct_num(input$ocf_local)
      )
    })
    
    num_or0 <- function(x) {
      v <- suppressWarnings(as.numeric(x))
      if (!length(v) || !is.finite(v[1])) return(0)
      v[1]
    }
    pct_or0 <- function(x) {
      v <- suppressWarnings(as.numeric(x))
      if (!length(v) || !is.finite(v[1])) return(0)
      v[1] / 100
    }
    
    cap_in <- reactive({
      scope <- if (is.null(input$cost_of_capital)) "Local" else input$cost_of_capital
      
      list(
        cost_scope = scope,
        group_distribution = cap_group()$distribution,
        group_cost         = cap_group()$cost,
        local_distribution  = cap_local()$distribution,
        local_cost          = cap_local()$cost,
        tax_group = pct_or0(input$tax_group %||% 28),
        tax_local = pct_or0(input$tax_local %||% 28),
        fund_group = pct_or0(input$ocf_group %||% 1.47),
        fund_local = pct_or0(input$ocf_local %||% 1.47)
      )
    })
    
    weighted_over_excel <- function(w1, w2, w3, w4, c1, c2, c3, c4, include_snp = TRUE) {
      w1 <- suppressWarnings(as.numeric(w1)); if (!length(w1) || !is.finite(w1)) w1 <- 0
      w2 <- suppressWarnings(as.numeric(w2)); if (!length(w2) || !is.finite(w2)) w2 <- 0
      w3 <- suppressWarnings(as.numeric(w3)); if (!length(w3) || !is.finite(w3)) w3 <- 0
      w4 <- suppressWarnings(as.numeric(w4)); if (!length(w4) || !is.finite(w4)) w4 <- 0
      c1 <- suppressWarnings(as.numeric(c1)); if (!length(c1) || !is.finite(c1)) c1 <- 0
      c2 <- suppressWarnings(as.numeric(c2)); if (!length(c2) || !is.finite(c2)) c2 <- 0
      c3 <- suppressWarnings(as.numeric(c3)); if (!length(c3) || !is.finite(c3)) c3 <- 0
      c4 <- suppressWarnings(as.numeric(c4)); if (!length(c4) || !is.finite(c4)) c4 <- 0
      if (w1 <= 0) return(0)
      num <- (w1*c1) + (w2*c2) + (w3*c3) + if (isTRUE(include_snp)) (w4*c4) else 0
      num / w1
    }
    
    compute_tax_gross <- function(scope, gd, gc, ld, lc) {
      if (identical(scope, "Group")) {
        D5 <- unname(gd[1]); D6 <- unname(gd[2]); D7 <- unname(gd[3]); D8 <- unname(gd[4])
        D9 <- unname(gc[1]); D10 <- unname(gc[2]); D11 <- unname(gc[3]); D12 <- unname(gc[4])
      } else {
        D5 <- unname(ld[1]); D6 <- unname(ld[2]); D7 <- unname(ld[3]); D8 <- unname(ld[4])
        D9 <- unname(lc[1]); D10 <- unname(lc[2]); D11 <- unname(lc[3]); D12 <- unname(lc[4])
      }
      D13 <- weighted_over_excel(D5,D6,D7,D8,D9,D10,D11,D12, include_snp = TRUE)
      D14 <- weighted_over_excel(D5,D6,D7,D8,D9,D10,D11,D12, include_snp = FALSE)
      list(D5=D5,D6=D6,D7=D7,D8=D8,D9=D9,D10=D10,D11=D11,D12=D12,D13=D13,D14=D14)
    }
    
    compute_net_of_tax_no_funding <- function(D, tax_group, tax_local, scope) {
      tax <- if (identical(scope, "Group")) tax_group else tax_local
      E5 <- D$D5; E6 <- D$D6; E7 <- D$D7; E8 <- D$D8
      E9  <- D$D9
      E10 <- D$D10 * (1 - tax)
      E11 <- D$D11 * (1 - tax)
      E12 <- D$D12 * (1 - tax)
      E13 <- weighted_over_excel(E5,E6,E7,E8,E9,E10,E11,E12, include_snp = TRUE)
      E14 <- weighted_over_excel(E5,E6,E7,E8,E9,E10,E11,E12, include_snp = FALSE)
      list(E5=E5,E6=E6,E7=E7,E8=E8,E9=E9,E10=E10,E11=E11,E12=E12,E13=E13,E14=E14,
           E26 = E13)
    }
    
    compute_net_of_tax_with_funding <- function(D, E, tax_group, tax_local, scope, fund_group, fund_local) {
      tax <- if (identical(scope, "Group")) tax_group else tax_local
      D16 <- if (identical(scope, "Group")) fund_group else fund_local
      k   <- D16 * (1 - tax)
      
      F5 <- D$D5; F6 <- D$D6; F7 <- D$D7; F8 <- D$D8
      F9  <- E$E9  - k
      F10 <- E$E10 - k
      F11 <- E$E11 - k
      F12 <- E$E12 - k
      
      F13 <- weighted_over_excel(F5,F6,F7,F8,F9,F10,F11,F12, include_snp = TRUE)
      F14 <- weighted_over_excel(F5,F6,F7,F8,F9,F10,F11,F12, include_snp = FALSE)
      list(F5=F5,F6=F6,F7=F7,F8=F8,F9=F9,F10=F10,F11=F11,F12=F12,F13=F13,F14=F14,
           G26 = F13, D16 = D16)
    }
    
    cap_res <- reactive({
      x <- cap_in()
      D <- compute_tax_gross(
        x$cost_scope, x$group_distribution, x$group_cost, x$local_distribution, x$local_cost
      )
      E <- compute_net_of_tax_no_funding(D, x$tax_group, x$tax_local, x$cost_scope)
      F <- compute_net_of_tax_with_funding(D, E, x$tax_group, x$tax_local,
                                           x$cost_scope, x$fund_group, x$fund_local)
      list(E26 = E$E26, G26 = F$G26, D16 = F$D16)
    })
    
    fmt_pct <- function(x) sprintf('%.4f%%', 100 * x)
    
    # ------------------------------------------------------------------
    # Capital Cost Inputs table (matches Excel "Capital Cost Inputs")
    # ------------------------------------------------------------------
    
    # Return full capital blocks so we can pick E5..E12 and F9..F12
    cap_blocks <- reactive({
      x <- cap_in()
      
      D <- compute_tax_gross(
        x$cost_scope, x$group_distribution, x$group_cost,
        x$local_distribution, x$local_cost
      )
      E <- compute_net_of_tax_no_funding(D, x$tax_group, x$tax_local, x$cost_scope)
      F <- compute_net_of_tax_with_funding(
        D, E, x$tax_group, x$tax_local, x$cost_scope, x$fund_group, x$fund_local
      )
      
      list(D = D, E = E, F = F)
    })
    
    output$capital_ratios_tbl <- render_pro_table_outputs({
      blk <- cap_blocks(); req(blk)
      D <- blk$D
      E <- blk$E
      F <- blk$F
      
      # Excel-like percent formatting: two decimals + % sign
      fmt_pct2 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        if (!length(v) || !is.finite(v[1])) return("")
        sprintf("%.2f%%", 100 * v[1])
      }
      
      # Build a table whose cell positions match your Excel screenshot:
      # - Leftmost column contains row labels (like col B in Excel)
      # - Then 3 value columns: Tax Gross, Net of Tax (not considering funding benefit), Net Of Tax (netting funding benefit)
      df <- data.frame(
        `Capital ratios` = c(
          "CET1 as % RWAs",
          "AT1 as % RWAs",
          "Tier 2 as % RWAs",
          "Senior Non Preferred as % RWAs",
          "Cost of CET1",
          "Cost of AT1",
          "Cost of Tier 2",
          "Cost of Senior Non Preferred",
          "Cost of Capital w. SNP",
          "Cost of Capital w/o SNP"
        ),
        `Tax Gross` = c(
          fmt_pct2(D$D5),
          fmt_pct2(D$D6),
          fmt_pct2(D$D7),
          fmt_pct2(D$D8),
          fmt_pct2(D$D9),
          fmt_pct2(D$D10),
          fmt_pct2(D$D11),
          fmt_pct2(D$D12),
          fmt_pct2(D$D13),
          fmt_pct2(D$D14)
        ),
        `Net of Tax (not considering funding benefit)` = c(
          fmt_pct2(E$E5),
          fmt_pct2(E$E6),
          fmt_pct2(E$E7),
          fmt_pct2(E$E8),
          fmt_pct2(E$E9),
          fmt_pct2(E$E10),
          fmt_pct2(E$E11),
          fmt_pct2(E$E12),
          fmt_pct2(E$E13),
          fmt_pct2(E$E14)
        ),
        `Net Of Tax (netting funding benefit)` = c(
          fmt_pct2(F$F5),
          fmt_pct2(F$F6),
          fmt_pct2(F$F7),
          fmt_pct2(F$F8),
          fmt_pct2(F$F9),
          fmt_pct2(F$F10),
          fmt_pct2(F$F11),
          fmt_pct2(F$F12),
          fmt_pct2(F$F13),
          fmt_pct2(F$F14)
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      df <- apply_pro_table_ui_outputs(df)
      
      df
    })
    
    # Numeric (unformatted) version of Capital Cost Inputs for EconCore
    # Returns fractions (e.g. 0.0345) NOT "3.45%" strings.
    capital_cost_inputs_num <- reactive({
      blk <- cap_blocks(); req(blk)
      E <- blk$E
      F <- blk$F
      
      funding_benefit_yes <- isTRUE(input$funding_benefit)
      
      # Costs: use funding-benefit switch like the displayed table does:
      # if Yes => F9..F12 else E9..E12
      cost_cet1 <- if (funding_benefit_yes) F$F9  else E$E9
      cost_at1  <- if (funding_benefit_yes) F$F10 else E$E10
      cost_t2   <- if (funding_benefit_yes) F$F11 else E$E11
      cost_snp  <- if (funding_benefit_yes) F$F12 else E$E12
      
      # Ratios ALWAYS come from E5..E8 in your UI table (matches what you show)
      ratio_cet1 <- E$E5
      ratio_at1  <- E$E6
      ratio_t2   <- E$E7
      ratio_snp  <- E$E8
      
      data.frame(
        Item  = c("CET1", "AT1", "Tier 2", "Senior Non Preferred"),
        Cost  = c(cost_cet1, cost_at1, cost_t2, cost_snp),      # fractions
        Ratio = c(ratio_cet1, ratio_at1, ratio_t2, ratio_snp),  # fractions
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    
    fmt_pct_2 <- function(x) {
      v <- suppressWarnings(as.numeric(x))
      if (!length(v) || !is.finite(v[1])) return("")
      sprintf("%.2f%%", 100 * v[1])
    }
    
    output$capital_cost_inputs_tbl <- renderTable({
      blk <- cap_blocks(); req(blk)
      E <- blk$E
      F <- blk$F
      
      # Excel $C$25: Funding Benefit? (TRUE => "Yes")
      funding_benefit_yes <- isTRUE(input$funding_benefit)
      
      # Funding row: CoF selected by scope (Group vs Local)
      scope <- input$cost_of_capital %||% "Local"
      cof <- if (identical(scope, "Group")) cap_in()$fund_group else cap_in()$fund_local
      
      # Costs column: IF funding benefit Yes then use F9..F12 else E9..E12
      cost_cet1 <- if (funding_benefit_yes) F$F9  else E$E9
      cost_at1  <- if (funding_benefit_yes) F$F10 else E$E10
      cost_t2   <- if (funding_benefit_yes) F$F11 else E$E11
      cost_snp  <- if (funding_benefit_yes) F$F12 else E$E12
      
      data.frame(
        `Capital Cost Inputs Item` = c("CET1", "AT1", "Tier 2", "Senior Non Preferred", "Funding"),
        Costs  = c(
          fmt_pct_2(cost_cet1),
          fmt_pct_2(cost_at1),
          fmt_pct_2(cost_t2),
          fmt_pct_2(cost_snp),
          fmt_pct_2(cof)
        ),
        Ratios = c(
          fmt_pct_2(E$E5),
          fmt_pct_2(E$E6),
          fmt_pct_2(E$E7),
          fmt_pct_2(E$E8),
          "" # Funding row has blank ratio in the screenshot
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }, rownames = FALSE)
    
    capital_cost_inputs_df <- reactive({
      blk <- cap_blocks(); req(blk)
      E <- blk$E
      F <- blk$F
      
      funding_benefit_yes <- isTRUE(input$funding_benefit)
      
      scope <- input$cost_of_capital %||% "Local"
      cof <- if (identical(scope, "Group")) cap_in()$fund_group else cap_in()$fund_local
      
      cost_cet1 <- if (funding_benefit_yes) F$F9  else E$E9
      cost_at1  <- if (funding_benefit_yes) F$F10 else E$E10
      cost_t2   <- if (funding_benefit_yes) F$F11 else E$E11
      cost_snp  <- if (funding_benefit_yes) F$F12 else E$E12
      
      # Keep same labels as the rendered table to avoid mapping ambiguity
      data.frame(
        `Capital Cost Inputs Item` = c("CET1", "AT1", "Tier 2", "Senior Non Preferred", "Funding"),
        Costs  = c(cost_cet1, cost_at1, cost_t2, cost_snp, cof),  # numeric FRACTIONS (0.1234)
        Ratios = c(E$E5, E$E6, E$E7, E$E8, NA_real_),             # numeric FRACTIONS (0.12)
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    
    # Numeric version of Capital Cost Inputs (fractions, not "xx.xx%" strings)
    capital_cost_inputs_num_df <- reactive({
      blk <- cap_blocks(); req(blk)
      E <- blk$E
      F <- blk$F
      
      funding_benefit_yes <- isTRUE(input$funding_benefit)
      
      cost_cet1 <- if (funding_benefit_yes) F$F9  else E$E9
      cost_at1  <- if (funding_benefit_yes) F$F10 else E$E10
      cost_t2   <- if (funding_benefit_yes) F$F11 else E$E11
      cost_snp  <- if (funding_benefit_yes) F$F12 else E$E12
      
      data.frame(
        Item  = c("CET1", "AT1", "Tier 2", "Senior Non Preferred"),
        Cost  = c(as.numeric(cost_cet1), as.numeric(cost_at1), as.numeric(cost_t2), as.numeric(cost_snp)),
        Ratio = c(as.numeric(E$E5),      as.numeric(E$E6),      as.numeric(E$E7),    as.numeric(E$E8)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
    
    
    output$cap_summary_table <- renderTable({
      r <- cap_res(); req(r)
      data.frame(
        Metric = c("Scope (Local/Group)",
                   "Cost of funding (D16)",
                   "Overall cost (no funding) (E26)",
                   "Overall cost (with funding) (G26)"),
        Value  = c(cap_in()$cost_scope, fmt_pct(r$D16), fmt_pct(r$E26), fmt_pct(r$G26)),
        check.names = FALSE
      )
    }, rownames = FALSE)
    
    observe({
      r <- cap_res(); req(r)
      ci <- cap_in()
      
      safe1 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        if (!length(v) || !is.finite(v[1])) 0 else v[1]
      }
      
      bus("capital_summary", list(
        scope = ci$cost_scope %||% "Local",
        E26   = safe1(r$E26),
        G26   = safe1(r$G26),
        D16   = safe1(r$D16)
      ))
    })
    
    output$tranche_DF_hot_ui <- renderUI({
      if (isTRUE(input$trancheInputs)) {
        div(
          rhandsontable::rHandsontableOutput(ns("tranche_DF_hot"), width = "100%"),
          div(style = "height: 36px;")
        )
      }
    })
    
    output$tranche_DF_hot <- rhandsontable::renderRHandsontable({
      req(isTRUE(input$trancheInputs))
      df <- hot_df()
      current_cols <- names(df)
      col_headers  <- pretty_headers(current_cols)
      build_hot(df, col_headers)
    })
    
    output$tranchedf_summary <- renderText({
      df <- tranche_df_prepared(); req(df)
      total_notional <- sum(df$Notional, na.rm = TRUE)
      paste0("Total Notional = ",
             format(round(total_notional, 0), big.mark = ",", scientific = FALSE))
    })
    
    output$tranchedf_cap_total <- renderText({
      df <- tranche_df_prepared(); req(df)
      cap_total <- sum(df$`Capital structure`, na.rm = TRUE)
      paste0("Capital structure total = ",
             sprintf("%.1f%%", 100 * cap_total))
    })
    
    amort_df <- reactive({
      scen <- scenario_df();   req(scen)
      tr0  <- tranche_df();    req(tr0)
      
      nms <- .normalise_names(names(scen))
      
      per_idx <- which(nms == "period")
      req(length(per_idx) >= 1)
      period <- scen[[per_idx[1]]]
      
      ref_idx <- which(nms == "reference portfolio balance")
      if (!length(ref_idx)) {
        ref_idx <- grep("^reference portfolio balance", nms)
      }
      req(length(ref_idx) >= 1)
      ref_bal <- scen[[ref_idx[1]]]
      
      letters_vec <- c("a","b","c","d","e","f")
      tranche_cols <- list()
      for (letter in letters_vec) {
        pat <- paste0("class ", letter, " balance")
        idx <- which(nms == pat)
        if (length(idx)) tranche_cols[[letter]] <- idx[1]
      }
      max_tranches <- length(tranche_cols)
      rv$max_tranches_from_scenario <- max_tranches
      
      class_ids <- if ("ClassID" %in% names(tr0)) tr0$ClassID else toupper(substr(tr0$Tranche, nchar(tr0$Tranche), nchar(tr0$Tranche)))
      class_ids <- as.character(class_ids)
      
      out <- data.frame(
        Period = period,
        `Reference portfolio balance` = ref_bal,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      if (length(class_ids) && max_tranches > 0) {
        for (i in seq_along(class_ids)) {
          letter <- tolower(class_ids[i])
          idx <- tranche_cols[[letter]]
          if (!is.null(idx)) {
            col_vals <- scen[[idx]]
            col_name <- if (!is.null(rv$tranche_names) && length(rv$tranche_names) >= i) {
              rv$tranche_names[i]
            } else {
              tr0$Tranche[i]
            }
            out[[col_name]] <- col_vals
          }
        }
      }
      
      out
    })
    
    output$amort_table <- rhandsontable::renderRHandsontable({
      df <- amort_df(); req(df)
      
      ht <- rhandsontable::rhandsontable(
        df,
        rowHeaders = NULL,
        width = "100%",
        height = 400
      )
      
      ht <- ht |> rhandsontable::hot_col(1, format = "0", readOnly = TRUE)
      
      if (ncol(df) >= 2) {
        for (j in 2:ncol(df)) {
          ht <- ht |> rhandsontable::hot_col(j, format = "0,0", readOnly = TRUE)
        }
      }
      
      ht |> rhandsontable::hot_table(
        contextMenu = FALSE,
        readOnly = TRUE,
        stretchH = "all"
      )
    })
    
    # -----------------------------
    # Loss vectors (new, scenario-driven, read-only)
    # -----------------------------
    loss_df <- reactive({
      scen <- scenario_df(); req(scen)
      tr0  <- tranche_df();  req(tr0)
      
      nms <- .normalise_names(names(scen))
      
      per_idx <- which(nms == "period")
      req(length(per_idx) >= 1)
      period <- scen[[per_idx[1]]]
      
      ref_loss_idx <- which(nms == "reference portfolio loss")
      if (!length(ref_loss_idx)) {
        ref_loss_idx <- grep("^reference portfolio loss", nms)
      }
      req(length(ref_loss_idx) >= 1)
      ref_loss <- scen[[ref_loss_idx[1]]]
      
      letters_vec <- c("a","b","c","d","e","f")
      loss_cols <- list()
      for (letter in letters_vec) {
        pat <- paste0("class ", letter, " loss")
        idx <- which(nms == pat)
        if (length(idx)) loss_cols[[letter]] <- idx[1]
      }
      max_loss_tranches <- length(loss_cols)
      
      class_ids <- if ("ClassID" %in% names(tr0)) tr0$ClassID else toupper(substr(tr0$Tranche, nchar(tr0$Tranche), nchar(tr0$Tranche)))
      class_ids <- as.character(class_ids)
      
      current_names <- if (!is.null(rv$tranche_names)) rv$tranche_names else tr0$Tranche
      n_tranches <- length(current_names)
      
      if (max_loss_tranches > 0 && n_tranches > max_loss_tranches) {
        current_names <- current_names[seq_len(max_loss_tranches)]
        class_ids     <- class_ids[seq_len(max_loss_tranches)]
        n_tranches    <- max_loss_tranches
      }
      
      out <- data.frame(
        Period = period,
        `Reference portfolio loss` = ref_loss,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      if (n_tranches > 0 && max_loss_tranches > 0) {
        for (i in seq_len(n_tranches)) {
          letter <- tolower(class_ids[i])
          idx <- loss_cols[[letter]]
          if (!is.null(idx)) {
            col_vals <- scen[[idx]]
            col_name <- paste0(current_names[i], " loss")
            out[[col_name]] <- col_vals
          }
        }
      }
      
      out
    })
    
    output$loss_table <- rhandsontable::renderRHandsontable({
      df <- loss_df(); req(df)
      
      ht <- rhandsontable::rhandsontable(
        df,
        rowHeaders = NULL,
        width = "100%",
        height = 400
      )
      
      ht <- ht |> rhandsontable::hot_col(1, format = "0", readOnly = TRUE)
      
      if (ncol(df) >= 2) {
        for (j in 2:ncol(df)) {
          ht <- ht |> rhandsontable::hot_col(j, format = "0,0", readOnly = TRUE)
        }
      }
      
      ht |> rhandsontable::hot_table(
        contextMenu = FALSE,
        readOnly = TRUE,
        stretchH = "all"
      )
    })
    
    # Capture user edits and keep Sold in sync with Mod95 by publishing back
    # --- NEW: prevent noisy re-publishes + sync Sold/Unfunded/Rating/Tranche to bus ---
    rv$last_tranche_modal_state <- NULL
    # --- NEW: debounce legacy tranche HOT input to prevent checkbox bouncing ---
    tranche_hot_raw <- reactive(input$tranche_DF_hot)
    tranche_hot_debounced <- shiny::debounce(tranche_hot_raw, millis = 250)
    
    
    # --- REPLACE: legacy tranche HOT observer (debounced + no bus echo loop) ---
    observeEvent(tranche_hot_debounced(), {
      
      hot_payload <- tranche_hot_debounced()
      edited <- tryCatch(rhandsontable::hot_to_r(hot_payload), error = function(e) NULL)
      if (is.null(edited) || !is.data.frame(edited)) return()
      
      df_up <- tranche_df()
      if (is.null(df_up) || nrow(df_up) != nrow(edited)) return()
      
      # pull values from legacy HOT
      sold_new <- if ("Sold" %in% names(edited)) suppressWarnings(as.numeric(edited$Sold)) else df_up$Sold.default
      unf_new  <- if ("Unfunded Protection?" %in% names(edited)) as.logical(edited[["Unfunded Protection?"]]) else df_up$Unfunded.default
      rat_new  <- if ("Rating" %in% names(edited)) as.character(edited$Rating) else df_up$Rating
      trn_new  <- if ("Tranche" %in% names(edited)) as.character(edited$Tranche) else df_up$Tranche
      # NEW: Coupon is editable in legacy tranche inputs and must sync back to bus
      cpn_new  <- if ("Coupon" %in% names(edited)) suppressWarnings(as.numeric(edited$Coupon)) else df_up$Coupon
      
      sold_new[!is.finite(sold_new)] <- df_up$Sold.default[!is.finite(sold_new)]
      unf_new[is.na(unf_new)] <- FALSE
      rat_new[is.na(rat_new)] <- ""
      trn_new[is.na(trn_new)] <- ""
      cpn_new[!is.finite(cpn_new)] <- df_up$Coupon[!is.finite(cpn_new)]
      
      # --- CRITICAL: avoid feedback loop ---
      # Only publish if values are actually different from current bus df.
      same_sold <- isTRUE(all.equal(df_up$Sold.default, sold_new, check.attributes = FALSE))
      same_unf  <- isTRUE(all.equal(as.logical(df_up$Unfunded.default), as.logical(unf_new), check.attributes = FALSE))
      same_rat  <- isTRUE(all.equal(as.character(df_up$Rating), as.character(rat_new), check.attributes = FALSE))
      same_trn  <- isTRUE(all.equal(as.character(df_up$Tranche), as.character(trn_new), check.attributes = FALSE))
      same_cpn  <- isTRUE(all.equal(as.numeric(df_up$Coupon), as.numeric(cpn_new), check.attributes = FALSE))
      
      if (same_sold && same_unf && same_rat && same_trn && same_cpn) {
        return()
      }
      
      # update bus source of truth
      df_up$Sold.default     <- sold_new
      df_up$Unfunded.default <- unf_new
      df_up$Rating           <- rat_new
      df_up$Tranche          <- trn_new
      df_up$Coupon <- cpn_new
      
      bus("tranche_df", df_up)
      
      # keep legacy vectors aligned (existing behavior)
      rv$sold <- sold_new
      rv$unfunded <- unf_new
      rv$tranche_names <- trn_new
      
    }, ignoreInit = TRUE)
    
    
    
    debug_outputs <- render_debug_outputs()
    output$dbg_tranche_df_head <- debug_outputs$dbg_tranche_df_head
    output$dbg_tranche_df_prepared_head <- debug_outputs$dbg_tranche_df_prepared_head
    output$dbg_hot_df_head <- debug_outputs$dbg_hot_df_head
    output$dbg_inputs_state <- debug_outputs$dbg_inputs_state
    
    user_dir <- "./user_saves/user_123/"
    if (!dir.exists(user_dir)) dir.create(user_dir, recursive = TRUE)
    file_list <- reactiveVal(list.files(user_dir, pattern = "\\.rds$"))
    refresh_file_list <- function() file_list(list.files(user_dir, pattern = "\\.rds$"))
    
    validate_mod90_shape <- function(obj) {
      # Must be a list saved by mod90_inputs()
      if (!is.list(obj)) return(FALSE)
      
      # Periods must exist (numeric-ish)
      if (is.null(obj$periods)) return(FALSE)
      
      # core_params should exist (but tolerate older versions)
      if (is.null(obj$core_params) || !is.list(obj$core_params)) return(FALSE)
      
      # tranche_modal may be missing in very early saves; tolerate but prefer it
      if (!is.null(obj$tranche_modal) && !is.list(obj$tranche_modal)) return(FALSE)
      
      # keys_inputs may exist; if present must be a list
      if (!is.null(obj$keys_inputs) && !is.list(obj$keys_inputs)) return(FALSE)
      
      # retained_positions may exist; if present must be list
      if (!is.null(obj$retained_positions) && !is.list(obj$retained_positions)) return(FALSE)
      
      TRUE
    }
    
    # ------------------------------------------------------------------
    # Econ (Base): "Notional amounts" block (Layer A – core, no RW yet)
    # ------------------------------------------------------------------
    econ_notional_base <- reactive({
      df_amort <- amort_df();    req(df_amort)
      df_tr0   <- tranche_df();  req(df_tr0)
      
      # Ensure we have sold / unfunded vectors aligned with current tranches
      n_tr <- nrow(df_tr0)
      
      sold_vec <- rv$sold
      if (is.null(sold_vec) || length(sold_vec) != n_tr) {
        init <- initialize_rv_values(df_tr0)
        sold_vec <- init$sold
      }
      
      unfunded_vec <- rv$unfunded
      if (is.null(unfunded_vec) || length(unfunded_vec) != n_tr) {
        init <- initialize_rv_values(df_tr0)
        unfunded_vec <- init$unfunded
      }
      
      unfunded_chr <- ifelse(
        is.na(unfunded_vec), NA_character_,
        ifelse(unfunded_vec, "Yes", "No")
      )
      
      tranche_keys <- compute_tranche_table(
        tranche        = df_tr0$Tranche,
        notional       = df_tr0$Notional,
        coupon         = df_tr0$Coupon,
        wal_zero_cpr   = df_tr0$WAL.scheduled,
        wal_with_cpr   = df_tr0$WAL.unscheduled,
        sold           = sold_vec,
        unfunded       = unfunded_chr
      )
      
      res <- compute_econ_notional_base(
        amort_df     = df_amort,
        tranche_keys = tranche_keys,
        n_additional = 3L
      )
      
      # IMPORTANT: do NOT relabel the columns here.
      # Keep the original period headers (1..120) so ERBA logic
      # sees exactly the same structure as in Excel.
      res
    })
    
    
    observe({
      # We want Econ Risk Weights period 0 values for Class A..F
      rw_tbl <- try(econ_riskweights_base(), silent = TRUE)
      if (inherits(rw_tbl, "try-error") || is.null(rw_tbl) || !is.data.frame(rw_tbl)) return()
      
      if (!("Label" %in% names(rw_tbl))) return()
      if (!("0" %in% names(rw_tbl))) return()
      
      class_order <- c("Class A","Class B","Class C","Class D","Class E","Class F")
      idx <- match(class_order, as.character(rw_tbl$Label))
      
      rw0 <- suppressWarnings(as.numeric(rw_tbl[["0"]][idx]))
      # keep NA as NA (compute_tranche_table handles NA)
      options(econ.d25_30 = rw0)
    })
    
    # NEW: tranche keys table (Key sheet tranche block) with Retained / Position etc.
    tranche_keys_base <- reactive({
      df_tr0 <- tranche_df(); req(df_tr0)
      
      n_tr <- nrow(df_tr0)
      
      sold_vec <- rv$sold
      if (is.null(sold_vec) || length(sold_vec) != n_tr) {
        init <- initialize_rv_values(df_tr0)
        sold_vec <- init$sold
      }
      
      unfunded_vec <- rv$unfunded
      if (is.null(unfunded_vec) || length(unfunded_vec) != n_tr) {
        init <- initialize_rv_values(df_tr0)
        unfunded_vec <- init$unfunded
      }
      
      unfunded_chr <- ifelse(
        is.na(unfunded_vec), NA_character_,
        ifelse(unfunded_vec, "Yes", "No")
      )
      
      compute_tranche_table(
        tranche        = df_tr0$Tranche,
        notional       = df_tr0$Notional,
        coupon         = df_tr0$Coupon,
        wal_zero_cpr   = df_tr0$WAL.scheduled,
        wal_with_cpr   = df_tr0$WAL.unscheduled,
        sold           = sold_vec,
        unfunded       = unfunded_chr
      )
    })
    
    observe({
      df <- tranche_keys_base()
      message("tranche_keys_base has Retained? ", "Retained" %in% names(df))
    })
    
    mezz_test_label_for_excel <- reactive({
      tk <- tranche_keys_base(); req(tk)
      
      if (!("Regulatory Position" %in% names(tk))) {
        # fallback to existing behavior if column is missing
        return(output_eba_tests_mezz_label(tk))
      }
      
      reg_pos <- trimws(as.character(tk$`Regulatory Position`))
      has_mezz <- any(reg_pos == "Mezzanine", na.rm = TRUE)
      
      if (isTRUE(has_mezz)) "Mezzanine Test" else "First Loss Test"
    })
    
    
    ## NEW: Econ (Base) summary block (blue rows above "Notional amounts")
    econ_summary_base <- reactive({
      econ_not_df <- econ_notional_base(); req(econ_not_df)
      
      # rv$risk_weight is already a FRACTION (e.g. 0.75 for 75%)
      rw_frac <- rv$risk_weight
      
      # Asset RW method (STD / IRB)
      asset_method <- input$asset_rw_method %||% "STD"
      
      # Provisions (%) from sidebar, converted to fraction using the same
      # helper used elsewhere in mod90.
      provisions_frac <- from_pct_num(input$provisions)
      
      # Clean-up Call (%) — currently a fixed 10% in the UI (Key!C32 in XLS).
      # If this later becomes a user input, only this line needs to change.
      cleanup_call_frac <- 0.10
      
      compute_econ_summary_base(
        econ_notional_df   = econ_not_df,
        risk_weight_frac   = rw_frac,
        asset_rw_method    = asset_method,
        provisions_frac    = provisions_frac,
        cleanup_call_frac  = cleanup_call_frac,
        eff_date           = input$effective_date,   # Date from Structure ▸ Dates
        time_call_date     = input$time_call_date,   # Time Call (if applicable)
        dcf                = dcf()                   # Frequency-derived DCF
      )
    })
    
    # Econ (Base): Risk Weights block (uses EconCore + ERBA RW Non-STS)
    econ_riskweights_base <- reactive({
      df_not <- econ_notional_base()
      df_rw  <- erba_monthly_rw_nonsts_base()
      
      compute_econ_risk_weights_base(
        econ_notional_df   = df_not,
        erba_rw_non_sts_base = df_rw
      )
    })
    
    
    # Small helper used for display only – relabel Econ period headers
    # from 1..N (internal) to 0..N-1 (UI), without touching Position/Label.
    econ_with_period0_headers <- function(df) {
      if (!is.data.frame(df)) return(df)
      
      period_cols <- setdiff(names(df), c("Position", "Label", "Aggregate"))
      if (!length(period_cols)) return(df)
      
      # Order columns numerically just in case
      nums <- suppressWarnings(as.numeric(period_cols))
      ok   <- !is.na(nums)
      if (!any(ok)) return(df)
      
      period_cols <- period_cols[ok][order(nums[ok])]
      new_names   <- as.character(seq.int(from = 0L, length.out = length(period_cols)))
      
      out <- df
      idx <- match(period_cols, names(out))
      names(out)[idx] <- new_names
      out
    }
    
    
    # Econ (Base): Risk-Weighted Exposure Amounts block
    econ_rwea_base <- reactive({
      df_not <- econ_notional_base();            req(df_not)
      df_erba <- erba_monthly_rw_nonsts_base();  req(df_erba)
      df_rw_tbl <- econ_riskweights_base();      req(df_rw_tbl)   # << add this
      
      compute_econ_rwea_base(
        econ_notional_df     = df_not,
        erba_rw_non_sts_base = df_erba,
        econ_riskweights_df  = df_rw_tbl         # << and this
      )
    })
    
    # Capital Components DF for EconCore (selected scope)
    capital_components_df_selected <- reactive({
      scope <- input$cost_of_capital %||% "Local"
      
      if (identical(scope, "Group")) {
        cap_df_to_hot(cap_group())
      } else {
        cap_df_to_hot(cap_local())
      }
    })
    
    
    econ_rw_exposure_base <- reactive({
      df_not <- econ_notional_base();    req(df_not)
      df_rw  <- econ_riskweights_base(); req(df_rw)
      df_tr  <- tranche_df();            req(df_tr)
      
      # Use the *actual* capital table based on selected scope
      cap_tbl <- if ((input$cost_of_capital %||% "Local") == "Group") {
        cap_df_to_hot(cap_group())
      } else {
        cap_df_to_hot(cap_local())
      }
      
      compute_econ_rw_exposure_base(
        econ_notional_df    = df_not,
        econ_riskweights_df = df_rw,
        tranche_df          = df_tr,
        fx_exposure         = (input$fx_exp %||% 0) / 100,
        fx_haircut          = (input$fx_haircut %||% 0) / 100,
        capital_components_df = cap_tbl,
        start_idx_for_p0    = 1L
      )
    })
    
    econ_nonmax_rw_exposure_base <- reactive({
      df_rwea <- econ_rwea_base();        req(df_rwea)
      df_rw   <- econ_riskweights_base(); req(df_rw)
      df_trk  <- tranche_keys_base();     req(df_trk)   # << THIS is the Key tranche table
      
      compute_econ_nonmax_rw_exposure_base(
        econ_rwea_df        = df_rwea,
        econ_riskweights_df = df_rw,
        tranche_table_df    = df_trk,
        start_idx_for_p0    = 1L,    # keep this (your tables are 1..N internally)
        rw_1250             = 1250
      )
    })
    
    
    # NEW: Exposure summary table (NonMax + Other + Total + Retained)
    econ_exposure_summary_base <- reactive({
      df_rwea  <- econ_rwea_base();              req(df_rwea)
      df_sum   <- econ_summary_base();           req(df_sum)
      df_rwexp <- econ_rw_exposure_base();       req(df_rwexp)
      df_nonmx <- econ_nonmax_rw_exposure_base();req(df_nonmx)
      
      # NEW inputs required for updated "RWEAs - Retained" formula
      df_rw    <- econ_riskweights_base();       req(df_rw)
      df_trk   <- tranche_keys_base();           req(df_trk)
      
      compute_econ_exposure_summary_base(
        econ_rwea_df               = df_rwea,
        econ_summary_base_df       = df_sum,
        econ_rw_exposure_base_df   = df_rwexp,
        econ_nonmax_rw_exposure_df = df_nonmx,
        econ_riskweights_df        = df_rw,
        tranche_table_df           = df_trk,
        rw_1250                    = 1250
      )
    })
    
    econ_rwea_reduction_base_stageA <- reactive({
      df_sum <- econ_summary_base();          req(df_sum)
      df_exp <- econ_exposure_summary_base(); req(df_exp)
      
      compute_econ_rwea_reduction_base_stageA(
        econ_summary_df          = df_sum,
        econ_exposure_summary_df = df_exp
      )
    })
    
    
    # NEW: Provisions / CET1 Capital effects table (right under RWEAs - Reduction)
    econ_cet1_capital_effects_base <- reactive({
      df_sum <- econ_summary_base(); req(df_sum)
      df_not <- econ_notional_base(); req(df_not)
      df_red <- econ_rwea_reduction_base_stageA(); req(df_red)
      df_trk <- tranche_keys_base(); req(df_trk)
      
      cap_tbl <- if ((input$cost_of_capital %||% "Local") == "Group") {
        cap_df_to_hot(cap_group())
      } else {
        cap_df_to_hot(cap_local())
      }
      
      compute_econ_cet1_capital_effects_base(
        econ_summary_df       = df_sum,
        econ_notional_df      = df_not,
        rwea_reduction_df     = df_red,
        tranche_table_df      = df_trk,
        capital_components_df = cap_tbl,
        asset_rw_method       = input$asset_rw_method %||% "STD",
        base_el_frac          = from_pct_num(input$base_pd) * from_pct_num(input$base_lgd),
        provisions_frac       = from_pct_num(input$provisions)
      )
    })
    
    econ_rwea_reduction_base <- reactive({
      df_sum  <- econ_summary_base();            req(df_sum)
      df_exp  <- econ_exposure_summary_base();   req(df_exp)
      
      # MUST exist before using the RWEA reduction now
      df_cet1 <- econ_cet1_capital_effects_base();       req(df_cet1)
      
      cap_tbl <- if ((input$cost_of_capital %||% "Local") == "Group") {
        cap_df_to_hot(cap_group())
      } else {
        cap_df_to_hot(cap_local())
      }
      
      compute_econ_rwea_reduction_base(
        econ_summary_df          = df_sum,
        econ_exposure_summary_df = df_exp,
        capital_components_df    = cap_tbl,
        econ_cet1_capital_df     = df_cet1
      )
    })
    
    econ_cost_of_protection_base <- reactive({
      df_sum <- econ_summary_base();  req(df_sum)
      df_not <- econ_notional_base(); req(df_not)
      df_trk <- tranche_keys_base();  req(df_trk)
      
      # Key!F26 (Overall Cost of Funding) mapping:
      # If scope == Group => input$ocf_group (%)
      # If scope == Local => input$ocf_local (%)
      scope <- input$cost_of_capital %||% "Local"
      overall_cost_f26 <- if (identical(scope, "Group")) {
        from_pct_num(input$ocf_group)   # e.g. 1.47% -> 0.0147
      } else {
        from_pct_num(input$ocf_local)   # e.g. 1.47% -> 0.0147
      }
      
      compute_econ_cost_of_protection_base(
        econ_summary_df         = df_sum,
        econ_notional_df        = df_not,
        tranche_table_df        = df_trk,
        dcf                     = dcf(),                              # must call reactive
        external_upfront        = input$external_upfront %||% 0,
        external_running        = input$external_running %||% 0,
        funding_benefit_flag    = isTRUE(input$funding_benefit),
        overall_cost_no_funding = overall_cost_f26,                   # Key!F26
        tax_group_frac          = from_pct_num(input$tax_group)        # Tax Rate for Group (%)
      )
    })
    
    econ_cost_of_protection_net_base <- reactive({
      df_sum <- econ_summary_base(); req(df_sum)
      df_cp  <- econ_cost_of_protection_base(); req(df_cp)   # existing table reactive you already wired
      df_trk <- tranche_keys_base(); req(df_trk)
      
      compute_econ_cost_of_protection_net_base(
        econ_summary_df   = df_sum,
        econ_cost_prot_df = df_cp,
        tranche_table_df  = df_trk
      )
    })
    
    econ_comparison_capital_cost_base <- reactive({
      df_red  <- econ_rwea_reduction_base();        req(df_red)
      df_cet1 <- econ_cet1_capital_effects_base();  req(df_cet1)
      cap_num <- capital_cost_inputs_num();         req(cap_num)
      
      compute_econ_comparison_capital_cost_base(
        econ_rwea_reduction_df       = df_red,
        econ_cet1_capital_df         = df_cet1,
        capital_cost_inputs_num_df   = cap_num,
        dcf                          = dcf()   # IMPORTANT: reactive call
      )
    })
    
    econ_ccc_deal_tests_base <- reactive({
      df_red  <- econ_rwea_reduction_base(); req(df_red)
      df_cet1 <- econ_cet1_capital_effects_base(); req(df_cet1)
      df_copn <- econ_cost_of_protection_net_base(); req(df_copn)
      
      # this is your existing comparison capital cost (base) table reactive
      df_ccc  <- econ_comparison_capital_cost_base(); req(df_ccc)
      
      df_cap_in <- capital_cost_inputs_num_df(); req(df_cap_in)
      
      compute_econ_ccc_deal_tests_base(
        econ_rwea_reduction_df          = df_red,
        econ_cet1_capital_df            = df_cet1,
        econ_cost_prot_net_df           = df_copn,
        econ_comparison_capital_cost_df = df_ccc,
        capital_cost_inputs_num_df      = df_cap_in
      )
    })
    
    ## Output Tab Reactives
    # ------------------------------------------------------------------
    # Outputs: EBA Summary table (with RW(SecRWMeth) + Charge(% Notional))
    # ------------------------------------------------------------------
    
    # --- Outputs tab: EBA Summary table data ---
    eba_summary_df <- reactive({
      tk  <- tranche_keys_base();         req(tk)
      rw  <- econ_riskweights_base();     req(rw)
      
      # SEC-IRBA Inputs sheet table is only required when method is SEC-IRBA,
      # but we can safely pass it always (NULL if not available)
      sec_tbl <- NULL
      if (exists("sec_irba_inputs_sheet_tbl") && is.function(sec_irba_inputs_sheet_tbl)) {
        sec_tbl <- try(sec_irba_inputs_sheet_tbl(), silent = TRUE)
        if (inherits(sec_tbl, "try-error")) sec_tbl <- NULL
      }
      
      # Base EBA summary (existing)
      df_eba <- output_build_eba_summary_df(
        tranche_keys_df          = tk,
        econ_riskweights_df      = rw,
        sec_rw_method            = input$sec_rw_method %||% "SEC-ERBA",
        sec_irba_inputs_sheet_df = sec_tbl
      )
      
      # ---- NEW dependency chain (as per your requirement) ----
      # 1) compute Test2 underlying row % first (needs UL + Lifetime EL in Test2)
      # 2) use that G54 to compute Allocated Risk in EBA Summary
      
      # Build Test2 base table (existing inputs)
      test2_base <- output_build_test2_commensurateness_tbl(
        sec_rw_method            = input$sec_rw_method %||% "SEC-ERBA",
        asset_rw_method          = input$asset_rw_method %||% "STD",
        keys_rw_pct              = (rv$risk_weight %||% 0) * 100,
        base_pd_pct              = input$base_pd %||% 0,
        base_lgd_pct             = input$base_lgd %||% 0,
        base_w_pct               = input$base_w %||% 0,
        excess_spread_annual_pct = input$excess_spread %||% 0,
        ptf_wal_years            = input$keys_wal %||% 0,
        eba_summary_df           = df_eba
      )
      
      # Fill underlying portfolio row (G54) in Test2
      test2_with_underlying <- output_test2_compute_underlying_el_plus_ul(
        test2_tbl_df    = test2_base,
        eba_summary_df   = df_eba
      )
      
      # Extract G54 = Standard % at the target row
      idx_g54 <- match("Lifetime EL + reg. UL on underlying portfolio", as.character(test2_with_underlying$Item))
      g54 <- if (!is.na(idx_g54)) suppressWarnings(as.numeric(test2_with_underlying$`Standard %`[idx_g54])) else 0
      if (!is.finite(g54)) g54 <- 0
      
      # Compute Allocated Risk column in EBA Summary using G54
      df_eba <- output_compute_allocated_risk(
        eba_summary_df                    = df_eba,
        lifetime_el_plus_ul_underlying_frac = g54
      )
      
      df_eba
    })
    
    observe({
      df_eba <- eba_summary_df()
      req(df_eba)
      
      # Toggle on/off quickly:
      debug_on <- FALSE
      if (!isTRUE(debug_on)) return()
      
      output_debug_test1_nominal_components(
        eba_summary_df = df_eba,
        sec_rw_method  = input$sec_rw_method %||% "SEC-ERBA"
      )
    })
    
    # ... RenderTable ...
    
    output$eba_summary_tbl <- render_pro_table_outputs({
      df <- eba_summary_df(); req(df)
      
      # Column name of the dynamic RW(method) column
      rw_meth_col <- output_rw_sec_erba_label(input$sec_rw_method %||% "SEC-ERBA")
      if (!(rw_meth_col %in% names(df))) rw_meth_col <- NULL
      
      # Identify the two extra rows
      extra_rows <- df$Tranche %in% c("Excess Spread", "Total Securitisation Capital")
      
      # Format helpers
      fmt_num0 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        ifelse(is.na(v), "-", format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE))
      }
      fmt_pct1 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        ifelse(is.na(v), "N/A", sprintf("%.1f%%", 100 * v))
      }
      fmt_rw_pct1 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        ifelse(is.na(v), "N/A", sprintf("%.1f%%", v))  # RW already in percent-units
      }
      fmt_rw_meth <- function(x) {
        x_chr <- trimws(as.character(x))
        x_chr[!nzchar(x_chr) | toupper(x_chr) %in% c("NA", "N/A")] <- NA_character_
        v <- suppressWarnings(as.numeric(x_chr))
        ifelse(is.finite(v), sprintf("%.1f%%", v), "N/A")
      }
      fmt_charge_pct2 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        ifelse(is.na(v), "N/A", sprintf("%.2f%%", 100 * v))
      }
      
      # Apply formatting (leave Tranche, Position as-is; format Notional as number)
      df$Notional <- fmt_num0(df$Notional)
      
      df$Attach              <- fmt_pct1(df$Attach)
      df$Detach              <- fmt_pct1(df$Detach)
      df$`Capital Structure` <- fmt_pct1(df$`Capital Structure`)
      df$Retained            <- fmt_pct1(df$Retained)
      
      df$RW <- fmt_rw_pct1(df$RW)
      
      if (!is.null(rw_meth_col)) {
        df[[rw_meth_col]] <- fmt_rw_meth(df[[rw_meth_col]])
      }
      
      df$`Charge (% Notional)` <- fmt_charge_pct2(df$`Charge (% Notional)`)
      
      # ---- Extra rows should be BLANK except what you intentionally filled ----
      if (any(extra_rows)) {
        # For extra rows: blank out all "N/A" and "-" produced by formatters
        df[extra_rows, ] <- lapply(df[extra_rows, , drop = FALSE], function(col) {
          col <- as.character(col)
          col[col %in% c("N/A", "-", "NA")] <- ""
          col
        })
        
        # But keep the Total row's Charge value (it will be formatted already)
        total_idx <- which(df$Tranche == "Total Securitisation Capital")
        if (length(total_idx) == 1) {
          # Re-format charge from the underlying numeric if needed
          # (If your Output.R already sets it, this line is fine as-is.)
          # Do nothing here—keeps whatever formatted value exists.
        }
      }
      
      if ("Allocated Risk" %in% names(df)) {
        df$`Allocated Risk` <- fmt_pct1(df$`Allocated Risk`)
      }
      
      df <- apply_pro_table_ui_outputs(df)
      df
    })
    
    
    # ------------------------------------------------------------------
    # Outputs: EBA TESTS header labels (Art. 244/245, Mezzanine Test, PASS)
    # ------------------------------------------------------------------
    
    eba_tests_header <- reactive({
      tk <- tranche_keys_base(); req(tk)
      
      output_build_eba_tests_header(
        trans_type_value = input$deal_type %||% "",
        tranche_keys_df  = tk
      )
    })
    
    outputs_c29_mezz_test_label <- reactive({
      hdr <- eba_tests_header(); req(hdr)
      as.character(hdr$test_label %||% "")
    })
    
    output$eba_tests_header_ui <- renderUI({
      hdr <- eba_tests_header(); req(hdr)
      rows_df <- eba_tests_rows_df(); req(rows_df)   # <- the 2 rows you already compute
      
      # Format like Excel screenshot:
      # Row 1 value is % with one decimal (e.g. 90.6%)
      # Row 2 value is PASS/FAIL
      fmt_pct1 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        if (!is.finite(v)) return("")
        sprintf("%.1f%%", 100 * v)
      }
      
      # Build formatted display values
      row_labels <- as.character(rows_df$Label %||% "")
      row_values <- as.character(rows_df$Value %||% "")
      
      if (length(row_values) >= 1) row_values[1] <- fmt_pct1(rows_df$Value[1])
      
      # A clean "card" header strip (no extra dependencies)
      tags$div(
        style = paste(
          "margin: 8px 0 10px 0;",
          "border: 1px solid #e5e7eb;",
          "border-radius: 10px;",
          "overflow: hidden;",
          "background: #ffffff;"
        ),
        
        # Top big heading row
        tags$div(
          style = paste(
            "padding: 12px 14px;",
            "background: #ffffff;",
            "color: #111827;",
            "border-bottom: 1px solid #e5e7eb;",
            "text-align: center;"
          ),
          tags$div(
            style = "font-size: 16px; font-weight: 700;",
            "EBA TESTS"
          )
        ),
        
        # Second row with the 3 header "cells" (Art / Test / PASS)
        tags$div(
          style = paste(
            "padding: 10px 14px;",
            "display: grid;",
            "grid-template-columns: 160px 1fr 120px;",
            "gap: 10px;",
            "align-items: center;"
          ),
          
          # Art label
          tags$div(
            style = paste(
              "background:#f1f5f9;",
              "border:1px solid #e5e7eb;",
              "border-radius:8px;",
              "padding:10px 12px;",
              "font-weight:700;",
              "text-align:left;"
            ),
            hdr$art_label %||% ""
          ),
          
          # Mezzanine Test / First Loss Test label
          tags$div(
            style = paste(
              "background:#f8fafc;",
              "border:1px solid #e5e7eb;",
              "border-radius:8px;",
              "padding:10px 12px;",
              "font-weight:700;"
            ),
            hdr$test_label %||% ""
          ),
          
          # PASS header label (right box)
          tags$div(
            style = paste(
              "background:#f1f5f9;",
              "border:1px solid #e5e7eb;",
              "border-radius:8px;",
              "padding:10px 12px;",
              "font-weight:700;",
              "text-align:center;"
            ),
            outputs_eba_tests_pass_label() %||% ""
          )
        ),
        
        # ------------------------------------------------------------------
        # NEW: The two rows rendered inside the SAME CARD, aligned to columns
        # ------------------------------------------------------------------
        tags$div(
          style = paste(
            "padding: 10px 14px 14px 14px;",
            "border-top: 1px solid #e5e7eb;"
          ),
          
          # Grid aligned with header columns: [Art col] [Test col] [PASS col]
          tags$div(
            style = paste(
              "display: grid;",
              "grid-template-columns: 160px 1fr 120px;",
              "gap: 10px;"
            ),
            
            # Row 1 (blank under Art col)
            tags$div(),
            tags$div(
              style = paste(
                "padding: 10px 12px;",
                "border: 1px solid #e5e7eb;",
                "border-radius: 0px;",  # Excel-like sharp edges
                "background: #ffffff;"
              ),
              row_labels[1] %||% ""
            ),
            tags$div(
              style = paste(
                "padding: 10px 12px;",
                "border: 1px solid #e5e7eb;",
                "border-radius: 0px;",
                "background: #ffffff;",
                "text-align: center;"
              ),
              row_values[1] %||% ""
            ),
            
            # Row 2 (blank under Art col)
            tags$div(),
            tags$div(
              style = paste(
                "padding: 10px 12px;",
                "border: 1px solid #e5e7eb;",
                "border-radius: 0px;",
                "background: #ffffff;",
                "font-weight: 700;" # Excel screenshot bolds the test condition row
              ),
              row_labels[2] %||% ""
            ),
            tags$div(
              style = paste(
                "padding: 10px 12px;",
                "border: 1px solid #e5e7eb;",
                "border-radius: 0px;",
                "text-align: center;",
                "font-weight: 700;",
                # mimic Excel green PASS cell
                if ((row_values[2] %||% "") == "PASS") "background:#16a34a; color:#ffffff;" else "background:#ffffff;"
              ),
              row_values[2] %||% ""
            )
          )
        )
      )
    })
    
    # ------------------------------------------------------------------
    # Outputs: Test 2) New Test of Commensurateness of the Risk Transfer
    # ------------------------------------------------------------------
    
    test2_commensurate_df <- reactive({
      df_eba <- eba_summary_df()
      req(df_eba)
      
      # 1) Build the base Test2 structure (Standard populated for the basic rows)
      test2 <- output_build_test2_commensurateness_tbl(
        sec_rw_method            = input$sec_rw_method %||% "SEC-ERBA",
        asset_rw_method          = input$asset_rw_method %||% "STD",
        keys_rw_pct              = (rv$risk_weight %||% 0) * 100,
        base_pd_pct              = input$base_pd %||% 0,
        base_lgd_pct             = input$base_lgd %||% 0,
        base_w_pct               = input$base_w %||% 0,
        excess_spread_annual_pct = input$excess_spread %||% 0,
        ptf_wal_years            = input$keys_wal %||% 0,
        eba_summary_df           = df_eba
      )
      
      # 2) Standard: underlying lifetime EL + reg UL (writes Standard % + Standard Value USD)
      test2 <- output_test2_compute_underlying_el_plus_ul(test2, df_eba)
      
      # 3) Allocated Risk depends on underlying % (Standard % row)
      underlying_frac <- suppressWarnings(as.numeric(
        test2$`Standard %`[match("Lifetime EL + reg. UL on underlying portfolio", test2$Item)]
      ))
      if (!is.finite(underlying_frac)) underlying_frac <- 0
      df_eba2 <- output_compute_allocated_risk(df_eba, underlying_frac)
      
      # 4) Standard: transferred lifetime EL + reg UL (needs tranche sold + allocated risk)
      tk <- tranche_keys_base()
      req(tk)
      test2 <- output_test2_compute_transferred_el_plus_ul(test2, tk, df_eba2)
      
      # 5) Standard: compute Ratio1/Ratio2/TestCondition/Result into Standard Value (USD)
      test2 <- output_test2_compute_standard_ratios_and_result(test2)
      
      # 6) IMPORTANT: populate ANNEX from Standard (so ANNEX cells are not blank)
      test2 <- output_test2_apply_annex_from_standard(test2)
      
      # 7) OPTIONAL: if you still want ANNEX ratio rows computed independently, do it now.
      # (Now it will work because ANNEX Value cells contain numeric USD)
      test2 <- output_test2_compute_annex_ratios_and_result(test2)
      
      # 8) Economic EL methodology (compute % and Value (USD) columns)
      test2 <- output_test2_compute_economic_el_section(
        test2_tbl_df             = test2,
        asset_rw_method          = input$asset_rw_method %||% "STD",
        keys_rw_pct              = input$risk_weighting_pct %||% 0,
        ptf_wal_years            = input$keys_wal %||% 0,
        excess_spread_annual_pct = input$excess_spread %||% 0,
        tranche_keys_df          = tk,
        eba_summary_df           = df_eba2
      )
      
      # 8) NEW: Fill the TOP "Result" row cells exactly like Excel
      test2 <- output_test2_fill_result_row(test2)
      
      test2
    })
    
    output$test2_commensurate_ui <- renderUI({
      df <- test2_commensurate_df(); req(df)
      
      fmt_pct2 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        if (!is.finite(v)) return("")
        sprintf("%.2f%%", 100 * v)
      }
      
      fmt_usd0 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        if (!is.finite(v)) return("")
        format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
      }
      
      fmt_num2 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        if (!is.finite(v)) return("")
        sprintf("%.2f", v)
      }
      
      # CSS only for this block (keeps rest of app unchanged)
      tags$div(
        tags$style(HTML("
      .test2-wrap { margin-top: 14px; border: 1px solid #111; }
      .test2-title {
        padding: 6px 10px;
        font-weight: 700;
        text-align: center;
        border-bottom: 1px solid #111;
      }
      table.test2-table { width: 100%; border-collapse: collapse; table-layout: fixed; }
      table.test2-table th, table.test2-table td {
        border: 1px solid #111;
        padding: 4px 6px;
        font-size: 13px;
        line-height: 1.1;
        vertical-align: middle;
      }
      table.test2-table th { font-weight: 700; background: #fff; }
      .test2-item { text-align: left; width: 42%; }
      .test2-pct  { text-align: center; width: 9.666%; }
      .test2-val  { text-align: right; width: 9.666%; }
      .test2-result-label { font-weight: 700; }
      .test2-section { font-weight: 700; }
      .test2-pass {
        background: #00b050;   /* Excel green */
        color: #fff;
        font-weight: 700;
        text-align: center !important;
      }
      .test2-center { text-align: center; }
    ")),
        
        tags$div(
          class = "test2-wrap",
          
          # Title row (no blue fill as requested)
          tags$div(
            class = "test2-title",
            "Test 2): New Test of Commensurateness of the Risk Transfer"
          ),
          
          # Table with "merged-like" headers using colspans
          tags$table(
            class = "test2-table",
            
            tags$thead(
              
              # Row 1: Group headers
              tags$tr(
                tags$th(class = "test2-item", ""),
                tags$th(class = "test2-center", colspan = 2, "Standard Methodology"),
                tags$th(class = "test2-center", colspan = 2, "ANNEX Methodology"),
                tags$th(class = "test2-center", colspan = 2, "Economic EL Methodology")
              ),
              
              # Row 3: Subheaders
              tags$tr(
                tags$th(class = "test2-center", "Item"),
                tags$th(class = "test2-center", "%"),
                tags$th(class = "test2-center", "Value (USD)"),
                tags$th(class = "test2-center", "%"),
                tags$th(class = "test2-center", "Value (USD)"),
                tags$th(class = "test2-center", "%"),
                tags$th(class = "test2-center", "Value (USD)")
              )
            ),
            
            tags$tbody(
              lapply(seq_len(nrow(df)), function(i) {
                item <- df$Item[i] %||% ""
                
                # Skip the first two rows because the header already renders "Result" + spacer
                # (df includes them for clarity, but UI handles result band in THEAD)
                if (i %in% c(1, 2)) return(NULL)
                
                # bold section rows (Ratio 1..., Ratio 2..., Test Condition...)
                is_section <- item %in% c(
                  "Ratio 1: Capital Reduction as a %",
                  "Ratio 2: Risk Transferred to Third Parties as a %",
                  "Test Condition: Ratio 1 <= Ratio 2"
                )
                
                # PASS cells only for last row (Test Condition...) value columns (like Excel)
                is_test_condition <- identical(item, "Test Condition: Ratio 1 <= Ratio 2")
                
                # Put this small formatter inside output$test2_commensurate_ui <- renderUI({ ... })
                fmt_pct2 <- function(x) {
                  v <- suppressWarnings(as.numeric(x))
                  if (!is.finite(v)) return("")
                  sprintf("%.2f%%", 100 * v)
                }
                fmt_cell_usd0_or_text <- function(x) {
                  # Excel-like:
                  # - if PASS/FAIL -> show as text
                  # - if numeric -> show 0dp with commas (no scientific)
                  # - else -> blank
                  if (is.null(x)) return("")
                  xc <- trimws(as.character(x))
                  if (!nzchar(xc) || xc %in% c("NA")) return("")
                  if (toupper(xc) %in% c("PASS", "FAIL")) return(toupper(xc))
                  
                  v <- suppressWarnings(as.numeric(xc))
                  if (!is.finite(v)) return("")
                  format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
                }
                
                fmt_cell_wal <- function(x) {
                  # WAL should be numeric with 2 decimals (e.g. 2.78)
                  v <- suppressWarnings(as.numeric(x))
                  if (!is.finite(v)) return("")
                  sprintf("%.2f", v)
                }
                
                tags$tr(
                  tags$td(class = paste("test2-item", if (is_section) "test2-section" else ""), item),
                  
                  tags$td(
                    class = "test2-pct",
                    {
                      item <- df$Item[i] %||% ""
                      if (item %in% c("WAL") || item %in% c("Result", "")) {
                        ""  # % blank for WAL/result spacer
                      } else {
                        fmt_pct2(df$`Standard %`[i])
                      }
                    }
                  ),
                  tags$td(
                    class = paste("test2-val", if (is_test_condition) "test2-pass" else ""),
                    {
                      item <- df$Item[i] %||% ""
                      val  <- df$`Standard Value (USD)`[i]
                      
                      # PASS/FAIL rows
                      if (toupper(trimws(as.character(val))) %in% c("PASS", "FAIL")) {
                        as.character(val)
                        
                        # WAL row is numeric (years)
                      } else if (identical(item, "WAL")) {
                        fmt_num2(val)
                        
                        # Ratio rows: Excel displays these as % (even though they sit under "Value (USD)")
                      } else if (item %in% c(
                        "Ratio 1: Capital Reduction as a %",
                        "Ratio 2: Risk Transferred to Third Parties as a %"
                      )) {
                        fmt_pct2(val)  # val is a fraction (0.5259 => 52.59%)
                        
                        # all other numeric rows are USD
                      } else {
                        fmt_usd0(val)
                      }
                    }
                  ),
                  tags$td(class = "test2-pct", fmt_pct2(df$`ANNEX %`[i])),
                  # ... inside output$test2_commensurate_ui <- renderUI({ ... })
                  
                  # Replace ONLY this ANNEX Value (USD) td block with the version below
                  tags$td(
                    class = paste("test2-val", if (is_test_condition) "test2-pass" else ""),
                    {
                      item <- df$Item[i] %||% ""
                      val  <- df$`ANNEX Value (USD)`[i]
                      
                      if (toupper(trimws(as.character(val))) %in% c("PASS", "FAIL")) {
                        toupper(trimws(as.character(val)))
                      } else if (identical(item, "WAL")) {
                        fmt_cell_wal(val)
                        
                        # Ratio rows must display as % (even though they sit under "Value (USD)")
                      } else if (item %in% c(
                        "Ratio 1: Capital Reduction as a %",
                        "Ratio 2: Risk Transferred to Third Parties as a %"
                      )) {
                        fmt_pct2(val)   # val is a fraction like 0.5259 -> "52.59%"
                        
                      } else {
                        fmt_cell_usd0_or_text(val)
                      }
                    }
                  ),
                  
                  tags$td(class = "test2-pct", fmt_pct2(df$`Economic EL %`[i])),
                  tags$td(
                    class = paste("test2-val", if (is_test_condition) "test2-pass" else ""),
                    {
                      item <- df$Item[i] %||% ""
                      val  <- df$`Economic EL Value (USD)`[i]
                      
                      if (toupper(trimws(as.character(val))) %in% c("PASS", "FAIL")) {
                        toupper(trimws(as.character(val)))
                      } else if (identical(item, "WAL")) {
                        fmt_cell_wal(val)
                      } else if (item %in% c(
                        "Ratio 1: Capital Reduction as a %",
                        "Ratio 2: Risk Transferred to Third Parties as a %"
                      )) {
                        fmt_pct2(val)
                      } else {
                        fmt_cell_usd0_or_text(val)
                      }
                    }
                  )
                )
              })
            )
          )
        )
      )
    })
    
    # ------------------------------------------------------------------
    # Outputs: Test 1) Minimum Thickness of the First Loss Tranche
    # ------------------------------------------------------------------
    
    test1_min_thickness_df <- reactive({
      df_eba <- eba_summary_df(); req(df_eba)
      df_t2  <- test2_commensurate_df(); req(df_t2)
      
      output_compute_test1_min_thickness_tbl(
        eba_summary_df        = df_eba,
        test2_df              = df_t2,
        mezz_test_label_value = outputs_c29_mezz_test_label(),   # $C$29 in Excel
        sec_rw_method         = input$sec_rw_method %||% "SEC-ERBA"
      )
    })
    
    output$test1_min_thickness_ui <- renderUI({
      df <- test1_min_thickness_df(); req(df)
      
      header_pf <- attr(df, "header_passfail")
      if (is.null(header_pf) || !nzchar(header_pf)) header_pf <- ""
      header_pf <- toupper(trimws(as.character(header_pf)))
      
      # Formatters (Excel-like)
      fmt_pct1 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        if (!is.finite(v)) return("")
        sprintf("%.1f%%", 100 * v)
      }
      fmt_usd0 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        if (!is.finite(v)) return("")
        format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
      }
      
      # Cell styles: PASS green, FAIL red (Excel-like)
      pass_style <- "background:#00b050; color:#ffffff; font-weight:700; text-align:center;"
      fail_style <- "background:#ff0000; color:#ffffff; font-weight:700; text-align:center;"
      pf_style <- function(x) {
        x <- toupper(trimws(as.character(x %||% "")))
        if (identical(x, "PASS")) return(pass_style)
        if (identical(x, "FAIL")) return(fail_style)
        ""
      }
      
      # Build the table (layout mirrors screenshot: title row with right PASS/FAIL cell)
      tags$div(
        tags$style(HTML("
      .test1-wrap { margin-top: 14px; border: 1px solid #111; }
      table.test1-table { width: 100%; border-collapse: collapse; table-layout: fixed; }
      table.test1-table th, table.test1-table td {
        border: 1px solid #111;
        padding: 3px 6px;
        font-size: 13px;
        line-height: 1.1;
        vertical-align: middle;
      }
      .test1-title { font-weight: 700; text-align: center; }
      .test1-item  { text-align: left;  width: 66%; }
      .test1-pct   { text-align: center; width: 12%; }
      .test1-usd   { text-align: right;  width: 22%; }
      .test1-bold  { font-weight: 700; }
      .test1-center{ text-align: center; }
    ")),
        
        tags$div(
          class = "test1-wrap",
          
          tags$table(
            class = "test1-table",
            
            tags$thead(
              # Row 1: big title + right PASS/FAIL cell
              tags$tr(
                tags$th(
                  class = "test1-title",
                  colspan = 2,
                  "Test 1): New Requirement for a Minimum Thickness of the First Loss Tranche"
                ),
                tags$th(
                  class = "test1-center",
                  style = pf_style(header_pf),
                  header_pf
                )
              ),
              
              # Row 2: column headers
              tags$tr(
                tags$th(class = "test1-center", "Item"),
                tags$th(class = "test1-center", "%"),
                tags$th(class = "test1-center", "Value (USD)")
              )
            ),
            
            tags$tbody(
              lapply(seq_len(nrow(df)), function(i) {
                item <- df$Item[i] %||% ""
                is_section <- item %in% c(
                  "Ratio 1 (Thickness of the First Loss Tranche):",
                  "Ratio 2 (Risk):",
                  "Test Condition: Ratio 1 >= Ratio 2"
                )
                
                val_pct <- df$`%`[i]
                val_usd <- df$`Value (USD)`[i]
                
                # format % and USD; keep blanks where Excel is blank
                pct_txt <- if (!nzchar(trimws(as.character(val_pct)))) "" else fmt_pct1(val_pct)
                
                # for the condition row, show PASS/FAIL as text (not number)
                usd_txt <- {
                  x <- trimws(as.character(val_usd))
                  if (!nzchar(x)) ""
                  else if (toupper(x) %in% c("PASS", "FAIL")) toupper(x)
                  else fmt_usd0(val_usd)
                }
                
                tags$tr(
                  tags$td(class = paste("test1-item", if (is_section) "test1-bold" else ""), item),
                  tags$td(class = "test1-pct", pct_txt),
                  tags$td(
                    class = paste("test1-usd", if (is_section && grepl("^Test Condition:", item)) "test1-bold" else ""),
                    # NEW: PASS green / FAIL red for the Test Condition row (and safe if usd_txt is PASS/FAIL)
                    style = pf_style(usd_txt),
                    usd_txt
                  )
                )
              })
            )
          )
        )
      )
    })
    
    # ------------------------------------------------------------------
    # Outputs: Test 3) New Comprehensive Test (UI scaffold)
    # ------------------------------------------------------------------
    
    # ------------------------------------------------------------------
    # Outputs: Test 3) New Comprehensive Test
    # ------------------------------------------------------------------
    
    test3_comprehensive_df <- reactive({
      # Use the already computed Test 2 table
      t2 <- test2_commensurate_df(); req(t2)
      
      tk <- tranche_keys_base(); req(tk)
      
      output_build_test3_comprehensive_tbl(
        test2_tbl_df      = t2,
        asset_rw_method   = input$asset_rw_method %||% "STD",
        keys_ref_notional = input$keys_ref_notional %||% 0,
        tranche_keys_df   = tk
      )
    })
    
    output$test3_comprehensive_ui <- renderUI({
      df <- test3_comprehensive_df(); req(df)
      
      # -----------------------
      # Display formatting helpers
      # -----------------------
      fmt_pct2 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        if (!is.finite(v)) return("")
        sprintf("%.2f%%", 100 * v)
      }
      
      fmt_val <- function(x, item_label = "") {
        xc <- trimws(as.character(x %||% ""))
        
        # Excel shows dash for blank numeric cells in this table
        if (!nzchar(xc) || xc %in% c("NA")) return("-")
        
        # PASS/FAIL stays as text
        if (toupper(xc) %in% c("PASS", "FAIL")) return(toupper(xc))
        
        v <- suppressWarnings(as.numeric(xc))
        if (!is.finite(v)) return("-")
        
        # Ka rows are shown as percentages (6.0%, 1.8%) in Value column
        if (item_label %in% c("Ka (Reg Min Risk Weight)", "30% of Ka Threshold")) {
          return(sprintf("%.1f%%", 100 * v))
        }
        
        # Default: USD integer with commas
        format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
      }
      
      # -----------------------
      # PASS/FAIL driving values
      # Header refers to Condition 2 test condition row (last row) Value (USD)
      # -----------------------
      header_pf <- toupper(trimws(as.character(df$`Value (USD)`[12] %||% "")))
      header_class <- if (header_pf == "PASS") "test3-pass" else if (header_pf == "FAIL") "test3-fail" else ""
      
      # Row PF classes (only for the two test condition rows)
      cond1_pf <- toupper(trimws(as.character(df$`Value (USD)`[7] %||% "")))
      cond2_pf <- toupper(trimws(as.character(df$`Value (USD)`[12] %||% "")))
      
      # -----------------------
      # Build table rows with TRUE Excel-like rowspans
      # Condition 1 spans rows 1..7, Condition 2 spans rows 8..12
      # -----------------------
      build_row <- function(i, include_group_cell = FALSE, group_label = "", group_rowspan = 1L) {
        item <- df$Item[i] %||% ""
        
        is_bold <- item %in% c(
          "Ratio 1: Capital on the Retained Tranches",
          "Ratio 2: Risk",
          "Test Condition: Ratio 1 <= Ratio 2",
          "Min Protection Required (Condition 2)",
          "Test Condition: Tranches sold >= min required *"
        )
        
        # PASS/FAIL colouring only for the condition rows (Value col)
        val_text <- fmt_val(df$`Value (USD)`[i], item_label = item)
        
        pf_class <- ""
        if (i == 7 && val_text %in% c("PASS", "FAIL")) pf_class <- if (val_text == "PASS") "test3-pass" else "test3-fail"
        if (i == 12 && val_text %in% c("PASS", "FAIL")) pf_class <- if (val_text == "PASS") "test3-pass" else "test3-fail"
        
        # % column only for rows (2,3,5,6) as per your computed structure
        pct_text <- if (i %in% c(2, 3, 5, 6)) fmt_pct2(df$`%`[i]) else ""
        
        tds <- tagList()
        
        if (include_group_cell) {
          tds <- tagAppendChildren(
            tds,
            tags$td(
              class = "test3-group",
              rowspan = group_rowspan,
              group_label
            )
          )
        }
        
        tds <- tagAppendChildren(
          tds,
          tags$td(class = paste("test3-item", if (is_bold) "test3-bold" else ""), item),
          tags$td(class = "test3-pct", pct_text),
          tags$td(
            class = paste(
              "test3-val",
              pf_class,
              if (is_bold && i %in% c(7, 12)) "test3-bold" else ""
            ),
            val_text
          )
        )
        
        tags$tr(tds)
      }
      
      # condition labels exactly like Excel
      cond1_label <- "Condition 1"
      cond2_label <- "Condition 2:"
      
      tags$div(
        tags$style(HTML("
      .test3-wrap { margin-top: 14px; }

      table.test3-table {
        width: 100%;
        border-collapse: collapse;
        table-layout: fixed;
        border: 1px solid #111;
        background: #fff;
      }

      table.test3-table th, table.test3-table td {
        border: 1px solid #111;
        padding: 2px 6px;        /* tighter like Excel */
        font-size: 13px;
        line-height: 1.05;
        vertical-align: middle;
        background: #fff;
      }

      table.test3-table th {
        font-weight: 700;
        text-align: center;
      }

      /* Column widths (match Excel proportions) */
      .test3-col-group { width: 14%; }
      .test3-col-item  { width: 58%; }
      .test3-col-pct   { width: 10%; }
      .test3-col-val   { width: 18%; }

      .test3-title-left {
        text-align: center;
        font-weight: 700;
      }
      .test3-title-right {
        text-align: center;
        font-weight: 700;
      }

      .test3-group {
        text-align: center;
        font-weight: 700;
        white-space: nowrap;
      }

      .test3-item { text-align: left; }
      .test3-pct  { text-align: center; }
      .test3-val  { text-align: right; }

      .test3-bold { font-weight: 700; }

      .test3-pass { background: #00b050 !important; color: #fff !important; font-weight: 700; text-align: center !important; }
      .test3-fail { background: #ff0000 !important; color: #fff !important; font-weight: 700; text-align: center !important; }

      /* Footnote outside table border */
      .test3-footnote {
        font-size: 12px;
        margin-top: 4px;
      }
    ")),
        
        tags$div(
          class = "test3-wrap",
          
          tags$table(
            class = "test3-table",
            
            tags$colgroup(
              tags$col(class = "test3-col-group"),
              tags$col(class = "test3-col-item"),
              tags$col(class = "test3-col-pct"),
              tags$col(class = "test3-col-val")
            ),
            
            tags$thead(
              # Title row: title spans first 3 cols, PASS/FAIL in last col
              tags$tr(
                tags$th(colspan = 3, class = "test3-title-left", "Test 3): New Comprehensive Test"),
                tags$th(class = paste("test3-title-right", header_class), header_pf)
              ),
              
              # Column headers row
              tags$tr(
                tags$th("", class = "test3-col-group"),
                tags$th("Item", class = "test3-col-item"),
                tags$th("%", class = "test3-col-pct"),
                tags$th("Value (USD)", class = "test3-col-val")
              )
            ),
            
            tags$tbody(
              # Condition 1 block (rowspan 7)
              build_row(1, include_group_cell = TRUE, group_label = cond1_label, group_rowspan = 7L),
              lapply(2:7, build_row),
              
              # Condition 2 block (rowspan 5)
              build_row(8, include_group_cell = TRUE, group_label = cond2_label, group_rowspan = 5L),
              lapply(9:12, build_row)
            )
          ),
          
          tags$div(class = "test3-footnote",
                   "* Risk transfer should come from tranches at higher risk")
        )
      )
    })
    econ_ccc_deal_tests_extra_labels_base <- reactive({
      # Inputs / tables needed for Excel-parity formulas
      df_sum <- econ_summary_base();      req(df_sum)
      df_not <- econ_notional_base();     req(df_not)
      df_rw  <- econ_riskweights_base();  req(df_rw)
      tk     <- tranche_keys_base();      req(tk)
      
      # Mechanics input: Excess Spread (Annual, %) -> fraction
      ex_spread_frac <- from_pct_num(input$excess_spread %||% 0)
      
      # Reference Portfolio Inputs: WAL (Yrs)
      ptf_wal <- suppressWarnings(as.numeric(input$keys_wal %||% 0))
      if (!is.finite(ptf_wal)) ptf_wal <- 0
      
      compute_econ_ccc_deal_tests_extra_labels_base(
        econ_summary_df            = df_sum,
        econ_notional_df           = df_not,
        econ_riskweights_df        = df_rw,
        tranche_table_df           = tk,
        outputs_mezz_test_label    = outputs_c29_mezz_test_label(),
        excess_spread_annual_frac  = ex_spread_frac,
        ptf_wal_years              = ptf_wal
      )
    })
    
    output$econ_ccc_deal_tests_extra_labels_base_tbl <- render_pro_table({
      df <- econ_ccc_deal_tests_extra_labels_base(); req(df)
      
      # Relabel headers 0..N-1 like your other UI tables
      df <- econ_with_period0_headers(df)
      
      period_cols <- setdiff(names(df), c("Label", "Aggregate"))
      
      is_life_es <- df$Label == "Lifetime Excess Spread"
      is_ratio1  <- grepl("^Mezzanine RWEAs Transferred", df$Label)
      is_test    <- df$Label == "Mezzanine Test"
      
      fmt_num0 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        v[!is.finite(v)] <- NA_real_
        ifelse(is.na(v), "", format(.excel_round0(v), big.mark = ",", scientific = FALSE, trim = TRUE))
      }
      fmt_pct1 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        v[!is.finite(v)] <- NA_real_
        ifelse(is.na(v), "", sprintf("%.1f%%", 100 * v))
      }
      
      for (pc in period_cols) {
        colv <- df[[pc]]
        
        # default: leave as-is (needed for PASS/FAIL row)
        out <- as.character(colv)
        
        # Lifetime Excess Spread -> number
        out[is_life_es] <- fmt_num0(colv[is_life_es])
        
        # Ratio1 -> percent
        out[is_ratio1] <- fmt_pct1(colv[is_ratio1])
        
        # Test row -> PASS/FAIL (already)
        out[is_test] <- as.character(colv[is_test])
        
        df[[pc]] <- out
      }
      
      df <- apply_pro_table_ui_keep_position(df)
      df
    })
    
    
    # ------------------------------------------------------------------
    # Econ (Base) — Regulatory EL + UL vs Non-senior tranches (NEW)
    # ------------------------------------------------------------------
    econ_reg_elul_nonsenior_test_base <- reactive({
      df_sum <- econ_summary_base();   req(df_sum)
      df_not <- econ_notional_base();  req(df_not)
      
      compute_econ_reg_elul_nonsenior_test_base(
        econ_summary_df = df_sum,
        econ_notional_df = df_not,
        base_pd_frac  = from_pct_num(input$base_pd %||% 0),   # Key!C10
        base_lgd_frac = from_pct_num(input$base_lgd %||% 0)   # Key!C11
      )
    })
    
    output$econ_reg_elul_nonsenior_test_base_tbl <- render_pro_table({
      df <- econ_reg_elul_nonsenior_test_base(); req(df)
      
      # Display formatting:
      # - Row 1 and 2 are fractions -> show as % with 1 decimal (Excel-like)
      # - Row 3 is PASS/FAIL text
      df <- econ_with_period0_headers(df)
      
      period_cols <- setdiff(names(df), c("Label", "Aggregate"))
      
      is_reg   <- df$Label == "Regulatory EL + UL"
      is_non   <- df$Label == "Non-senior tranches"
      is_test  <- df$Label == "Test"
      
      fmt_pct1 <- function(x) {
        v <- suppressWarnings(as.numeric(x))
        v[!is.finite(v)] <- NA_real_
        ifelse(is.na(v), "", sprintf("%.1f%%", 100 * v))
      }
      
      for (pc in period_cols) {
        colv <- df[[pc]]
        out <- as.character(colv)
        
        out[is_reg]  <- fmt_pct1(colv[is_reg])
        out[is_non]  <- fmt_pct1(colv[is_non])
        out[is_test] <- as.character(colv[is_test])
        
        df[[pc]] <- out
      }
      df <- apply_pro_table_ui_keep_position(df)
      df
    })
    
    # --- Outputs tab: Econ D89 analogue (Ratio 1, period 0) ---
    outputs_eba_tests_ratio1_p0 <- reactive({
      df <- econ_ccc_deal_tests_extra_labels_base(); req(df)
      
      # In the Economics UI you relabel period headers to 0..N-1, but
      # this compute function returns numeric columns already. We specifically want period 0.
      econ_extract_ratio1_period0(df)
    })
    
    # --- Outputs tab: Build the 2 EBA Tests rows (labels + values) ---
    eba_tests_rows_df <- reactive({
      output_build_eba_tests_rows(
        mezz_test_label_value = outputs_c29_mezz_test_label(),
        ratio1_value          = outputs_eba_tests_ratio1_p0()
      )
    })
    
    # --- Outputs tab: PASS label in header (Excel: =H33 i.e. PASS/FAIL from condition row) ---
    outputs_eba_tests_pass_label <- reactive({
      df <- eba_tests_rows_df(); req(df)
      # second row "Value" is PASS/FAIL
      as.character(df$Value[2] %||% "")
    })
    
    
    # -----------------------------------------------------------------
    # UPDATED assign_from_upload():
    #   - Restores all fields added to mod90_inputs() above
    #   - Defensive: handles missing fields (older saves)
    #   - Keeps tranche_df / Sold.default, capital HOTs, retained table in sync
    #   - Restores SRT subtab state (Economics / Amortisation / Loss vectors)
    #   - NEW: restores full tranche_state so mod95 edits are recovered
    # -----------------------------------------------------------------
    assign_from_upload <- function(uploaded) {
      if (!validate_mod90_shape(uploaded)) {
        showNotification("Invalid uploaded object for mod90.", type = "error")
        return(FALSE)
      }
      
      # Helper for numeric safety
      safe_num <- function(x, fallback = NULL) {
        v <- suppressWarnings(as.numeric(x))
        if (is.finite(v[1])) v[1] else fallback
      }
      
      # 1) Periods
      updateNumericInput(session, "periods", value = as.numeric(uploaded$periods)[1])
      
      # 2) Checkbox / toggle state (panels + subgroups)
      if (!is.null(uploaded$checkboxes)) {
        cb <- uploaded$checkboxes
        try(updateCheckboxInput(session, "portfolioInputs",       value = isTRUE(cb$portfolioInputs)),       silent = TRUE)
        try(updateCheckboxInput(session, "coreParams",            value = isTRUE(cb$coreParams)),            silent = TRUE)
        try(updateCheckboxInput(session, "riskParams",            value = isTRUE(cb$riskParams)),            silent = TRUE)
        try(updateCheckboxInput(session, "provFXParams",          value = isTRUE(cb$provFXParams)),          silent = TRUE)
        try(updateCheckboxInput(session, "capitalComponentsMain", value = isTRUE(cb$capitalInputs)),         silent = TRUE)
        try(updateCheckboxInput(session, "trancheInputs",         value = isTRUE(cb$trancheInputs)),         silent = TRUE)
        try(updateCheckboxInput(session, "regulatoryBox",         value = isTRUE(cb$regulatoryBox)),         silent = TRUE)
        try(updateCheckboxInput(session, "structureMain",         value = isTRUE(cb$structureMain)),         silent = TRUE)
        try(updateCheckboxInput(session, "dealInfo",              value = isTRUE(cb$dealInfo)),              silent = TRUE)
        try(updateCheckboxInput(session, "datesBox",              value = isTRUE(cb$datesBox)),              silent = TRUE)
        try(updateCheckboxInput(session, "mechanicsBox",          value = isTRUE(cb$mechanicsBox)),          silent = TRUE)
        try(updateCheckboxInput(session, "costsBox",              value = isTRUE(cb$costsBox)),              silent = TRUE)
        # NEW sub-checkboxes for Costs section
        if (!is.null(cb$externalCostsBox)) {
          try(updateCheckboxInput(session, "externalCostsBox", value = isTRUE(cb$externalCostsBox)), silent = TRUE)
        }
        if (!is.null(cb$taxRateBox)) {
          try(updateCheckboxInput(session, "taxRateBox", value = isTRUE(cb$taxRateBox)), silent = TRUE)
        }
        try(updateCheckboxInput(session, "retainedBox",           value = isTRUE(cb$retainedBox)),           silent = TRUE)
        try(updateCheckboxInput(session, "keys_link_tax_local",   value = isTRUE(cb$keys_link_tax_local)),   silent = TRUE)
        
        # NEW: SEC-IRBA subgroup toggle
        if (!is.null(cb$secIrbaInputsMain)) {
          try(updateCheckboxInput(session, "secIrbaInputsMain", value = isTRUE(cb$secIrbaInputsMain)), silent = TRUE)
        }
      }
      
      # 3) Core params (rv + visible inputs)
      if (!is.null(uploaded$core_params$currency)) update_currency(as.character(uploaded$core_params$currency))
      if (!is.null(uploaded$core_params$risk_weight)) {
        val <- suppressWarnings(as.numeric(uploaded$core_params$risk_weight))
        if (!is.na(val)) update_risk_w(val)
      }
      try(updateSelectInput(session, "currency_sel", selected = uploaded$core_params$currency), silent = TRUE)
      try(updateNumericInput(session, "risk_weighting_pct",
                             value = safe_num(uploaded$core_params$risk_weight * 100, 75)), silent = TRUE)
      
      # 4) Risk params
      if (!is.null(uploaded$risk_params)) {
        try(updateNumericInput(session, "base_pd",  value = uploaded$risk_params$base_pd),  silent = TRUE)
        try(updateNumericInput(session, "base_lgd", value = uploaded$risk_params$base_lgd), silent = TRUE)
        try(updateNumericInput(session, "base_w",   value = uploaded$risk_params$base_w),   silent = TRUE)
      }
      
      # 5) Provisions / FX
      if (!is.null(uploaded$provfx_params)) {
        try(updateNumericInput(session, "provisions", value = uploaded$provfx_params$provisions), silent = TRUE)
        try(updateNumericInput(session, "fx_exp",     value = uploaded$provfx_params$fx_exp),     silent = TRUE)
        try(updateNumericInput(session, "fx_haircut", value = uploaded$provfx_params$fx_haircut), silent = TRUE)
      }
      
      # 6) Regulatory
      if (!is.null(uploaded$regulatory)) {
        try(updateSelectInput(session, "asset_rw_method", selected = uploaded$regulatory$asset_rw_method), silent = TRUE)
        try(updateSelectInput(session, "sec_rw_method",   selected = uploaded$regulatory$sec_rw_method),   silent = TRUE)
        try(updateCheckboxInput(session, "sts",           value = isTRUE(uploaded$regulatory$sts)),        silent = TRUE)
        try(updateSelectInput(session, "pool_type",       selected = uploaded$regulatory$pool_type),       silent = TRUE)
        try(updateCheckboxInput(session, "funding_benefit", value = isTRUE(uploaded$regulatory$funding_benefit)), silent = TRUE)
        try(updateRadioButtons(session, "cost_of_capital",
                               selected = uploaded$regulatory$cost_of_capital %||% "Local"), silent = TRUE)
      }
      
      # 7) Structure (deal type, dates, mechanics)
      if (!is.null(uploaded$structure)) {
        try(updateSelectInput(session, "deal_type", selected = uploaded$structure$deal_type), silent = TRUE)
        try(updateDateInput(session, "effective_date",       value = uploaded$structure$effective_date),      silent = TRUE)
        try(updateDateInput(session, "final_maturity_date", value = uploaded$structure$final_maturity_date), silent = TRUE)
        try(updateDateInput(session, "time_call_date",      value = uploaded$structure$time_call_date),      silent = TRUE)
        try(updateSelectInput(session, "amort_type",  selected = uploaded$structure$amort_type),  silent = TRUE)
        try(updateNumericInput(session, "excess_spread", value = uploaded$structure$excess_spread), silent = TRUE)
        
        # NEW: restore Frequency (A/Q/M)
        if (!is.null(uploaded$structure$frequency)) {
          try(updateSelectInput(session, "frequency", selected = uploaded$structure$frequency), silent = TRUE)
        }
      }
      
      # 8) Costs & tax
      if (!is.null(uploaded$costs_tax)) {
        try(updateNumericInput(session, "external_upfront", value = uploaded$costs_tax$external_upfront), silent = TRUE)
        try(updateNumericInput(session, "external_running", value = uploaded$costs_tax$external_running), silent = TRUE)
        try(updateNumericInput(session, "tax_group",        value = uploaded$costs_tax$tax_group),        silent = TRUE)
        try(updateNumericInput(session, "tax_local",        value = uploaded$costs_tax$tax_local),        silent = TRUE)
        try(updateCheckboxInput(session, "keys_link_tax_local",
                                value = isTRUE(uploaded$costs_tax$keys_link_tax_local)), silent = TRUE)
      }
      
      # 9) Capital yellow (ocf + tables)
      if (!is.null(uploaded$capital_yellow)) {
        try(updateNumericInput(session, "ocf_group", value = uploaded$capital_yellow$ocf_group), silent = TRUE)
        try(updateNumericInput(session, "ocf_local", value = uploaded$capital_yellow$ocf_local), silent = TRUE)
        
        # Restore underlying reactive capital vectors
        if (!is.null(uploaded$capital_yellow$group_table)) {
          gt <- uploaded$capital_yellow$group_table
          cap_group(list(
            distribution = as.numeric(gt$`Distribution (% RWAs)`) / 100,
            cost         = as.numeric(gt$`Cost of Capital (%)`)   / 100
          ))
        }
        if (!is.null(uploaded$capital_yellow$local_table)) {
          lt <- uploaded$capital_yellow$local_table
          cap_local(list(
            distribution = as.numeric(lt$`Distribution (% RWAs)`) / 100,
            cost         = as.numeric(lt$`Cost of Capital (%)`)   / 100
          ))
        }
      }
      
      # 10) Keys-related numeric inputs
      if (!is.null(uploaded$keys_inputs)) {
        ki <- uploaded$keys_inputs
        try(updateNumericInput(session, "keys_ref_notional",       value = ki$keys_ref_notional),       silent = TRUE)
        try(updateNumericInput(session, "keys_wal",                value = ki$keys_wal),                silent = TRUE)
        try(updateNumericInput(session, "keys_cpr",                value = ki$keys_cpr),                silent = TRUE)
        try(updateNumericInput(session, "keys_n",                  value = ki$keys_n),                  silent = TRUE)
        try(updateNumericInput(session, "keys_longest_mat_months", value = ki$keys_longest_mat_months), silent = TRUE)
      }
      
      # NEW (between your steps 10 and 11): restore SEC-IRBA editable table values
      if (!is.null(uploaded$sec_irba_inputs$table) &&
          is.data.frame(uploaded$sec_irba_inputs$table) &&
          exists("sec_irba_inputs_rv") && is.function(sec_irba_inputs_rv)) {
        
        # Keep your original behaviour: do not change logic, only restore
        try(sec_irba_inputs_rv(uploaded$sec_irba_inputs$table), silent = TRUE)
      }
      
      # 11) Retained securitisation positions table
      if (!is.null(uploaded$retained_positions$table) &&
          is.data.frame(uploaded$retained_positions$table)) {
        rv$retained_positions <- uploaded$retained_positions$table
      } else {
        # Fall back to defaults if missing
        rv$retained_positions <- retained_defaults
      }
      
      # 12) Restore full tranche_state (structure + names) and tranche_modal
      #     so that mod95’s edits are fully recovered.
      df_saved <- NULL
      if (!is.null(uploaded$tranche_state$tranche_df) &&
          is.data.frame(uploaded$tranche_state$tranche_df)) {
        df_saved <- uploaded$tranche_state$tranche_df
        
        # Light validation: require Tranche & Notional columns
        if (!all(c("Tranche", "Notional") %in% names(df_saved))) {
          df_saved <- NULL
        }
      }
      
      if (!is.null(df_saved)) {
        # Publish saved tranche_df back onto bus so mod90 & mod95 both see it
        bus("tranche_df", df_saved)
      }
      
      # Determine number of tranches from restored df (if any)
      n_tranches <- if (!is.null(df_saved)) nrow(df_saved) else {
        tryCatch(nrow(tranche_df_prepared()), error = function(e) NA_integer_)
      }
      
      if (!is.na(n_tranches) && n_tranches > 0) {
        # Restore Sold & Unfunded from tranche_modal
        if (!is.null(uploaded$tranche_modal)) {
          tm <- uploaded$tranche_modal
          
          sold_in <- suppressWarnings(as.numeric(tm$sold))
          sold_in[is.na(sold_in)] <- 0
          sold_in <- pmax(0, pmin(1, sold_in))
          if (length(sold_in) < n_tranches) sold_in <- c(sold_in, rep(0, n_tranches - length(sold_in)))
          if (length(sold_in) > n_tranches) sold_in <- sold_in[seq_len(n_tranches)]
          rv$sold <- sold_in
          
          unf_in <- as.logical(tm$unfunded_protection)
          if (length(unf_in) < n_tranches) unf_in <- c(unf_in, rep(FALSE, n_tranches - length(unf_in)))
          if (length(unf_in) > n_tranches) unf_in <- unf_in[seq_len(n_tranches)]
          rv$unfunded <- unf_in
          
          # Optional saved tranche_names (for Amortisation / Loss vectors)
          if (!is.null(tm$tranche_names)) {
            tn <- as.character(tm$tranche_names)
            if (length(tn) >= n_tranches) {
              rv$tranche_names <- tn[seq_len(n_tranches)]
            } else {
              rv$tranche_names <- tn
            }
          }
        } else {
          rv$sold <- NULL
          rv$unfunded <- NULL
        }
        
        # Also, if tranche_state has saved names, prefer them (aligned to n_tranches)
        if (!is.null(uploaded$tranche_state$tranche_names)) {
          tn2 <- as.character(uploaded$tranche_state$tranche_names)
          if (length(tn2) >= n_tranches) {
            rv$tranche_names <- tn2[seq_len(n_tranches)]
          } else if (length(tn2) > 0) {
            rv$tranche_names <- tn2
          }
        }
        
        # If we have df_saved and rv$sold, keep Sold.default in sync and republish
        if (!is.null(df_saved) && !is.null(rv$sold) && nrow(df_saved) == length(rv$sold)) {
          df_saved$Sold.default <- rv$sold
          bus("tranche_df", df_saved)
        } else {
          # If df_saved is NULL but bus still has a df, update its Sold.default
          df_up <- tranche_df()
          if (!is.null(df_up) && !is.null(rv$sold) && nrow(df_up) == length(rv$sold)) {
            df_up$Sold.default <- rv$sold
            bus("tranche_df", df_up)
          }
        }
      } else {
        rv$sold <- NULL
        rv$unfunded <- NULL
      }
      
      # 13) Re-render capital HOTs if the capital panel is open
      if (isTRUE(input$capitalComponentsMain)) {
        output$cap_tbl_group_edit <- rhandsontable::renderRHandsontable({
          df <- cap_df_to_hot(cap_group())
          rhandsontable::rhandsontable(df, stretchH = "none", height = 160)
        })
        output$cap_tbl_local_edit <- rhandsontable::renderRHandsontable({
          df <- cap_df_to_hot(cap_local())
          rhandsontable::rhandsontable(df, stretchH = "none", height = 160)
        })
      }
      
      # 14) If tranche modal is open, re-render HOT with restored Sold/unfunded
      if (isTRUE(input$trancheInputs)) {
        output$tranche_DF_hot <- rhandsontable::renderRHandsontable({
          df <- hot_df()
          current_cols <- names(df)
          col_headers  <- pretty_headers(current_cols)
          build_hot(df, col_headers)
        })
      }
      
      # 15) Restore SRT subtab (Economics / Amortisation / Loss vectors)
      if (!is.null(uploaded$subtab_state$SRT_subtab)) {
        target_subtab <- uploaded$subtab_state$SRT_subtab
        try(updateTabsetPanel(session, "SRT_subtab", selected = target_subtab), silent = TRUE)
        rv$last_SRT_subtab <- target_subtab
      }
      
      showNotification("Inputs loaded into SRT (mod90).", type = "message")
      TRUE
    }
    
    saveUploadFun(
      input = input,
      session = session,
      object_to_save = mod90_inputs,
      save_trigger_id = "save_btn",
      upload_trigger_id = "modal_upload",
      default_name = "srt_mod90_session",
      user_dir = user_dir,
      accepted_classes = c("list"),
      file_list = file_list,
      refresh_file_list = refresh_file_list,
      validation_callback = validate_mod90_shape,
      assign_callback = assign_from_upload
    )
    
    observeEvent(input$trancheInputs, {
      if (isTRUE(input$trancheInputs)) manage_tranche_modal("open") else manage_tranche_modal("close")
    }, ignoreInit = TRUE)
    
    observeEvent(input$trancheCloseBtn, { manage_tranche_modal("close") }, ignoreInit = TRUE)
    
    
    reset_mod90_to_defaults <- function() {
      # 1) Tell siblings (mod95) to reset scenario/tranches
      bus("reset_srt", TRUE)
      
      # 2) Sidebar checkbox states (all groups + subgroups)
      cb <- .mod90_defaults$checkboxes
      updateCheckboxInput(session, "portfolioInputs",       value = cb$portfolioInputs)
      updateCheckboxInput(session, "coreParams",            value = cb$coreParams)
      updateCheckboxInput(session, "riskParams",            value = cb$riskParams)
      updateCheckboxInput(session, "provFXParams",          value = cb$provFXParams)
      updateCheckboxInput(session, "regulatoryBox",         value = cb$regulatoryBox)
      updateCheckboxInput(session, "structureMain",         value = cb$structureMain)
      updateCheckboxInput(session, "dealInfo",              value = cb$dealInfo)
      updateCheckboxInput(session, "datesBox",              value = cb$datesBox)
      updateCheckboxInput(session, "mechanicsBox",          value = cb$mechanicsBox)
      updateCheckboxInput(session, "trancheInputs",         value = cb$trancheInputs)
      updateCheckboxInput(session, "costsBox",              value = cb$costsBox)
      updateCheckboxInput(session, "externalCostsBox",      value = cb$externalCostsBox)  
      updateCheckboxInput(session, "taxRateBox",            value = cb$taxRateBox)        
      updateCheckboxInput(session, "capitalComponentsMain", value = cb$capitalComponentsMain)
      updateCheckboxInput(session, "retainedBox",           value = cb$retainedBox)
      updateCheckboxInput(session, "keys_link_tax_local",   value = cb$keys_link_tax_local)
      updateCheckboxInput(session, "secIrbaInputsMain",     value = cb$secIrbaInputsMain)
      
      # 3) All editable inputs (Portfolio)
      updateNumericInput(session, "periods", value = .mod90_defaults$periods())
      updateSelectInput(session, "currency_sel", selected = .mod90_defaults$currency_sel)
      updateNumericInput(session, "risk_weighting_pct", value = .mod90_defaults$risk_weighting_pct)
      updateNumericInput(session, "base_pd",  value = .mod90_defaults$base_pd)
      updateNumericInput(session, "base_lgd", value = .mod90_defaults$base_lgd)
      updateNumericInput(session, "base_w",   value = .mod90_defaults$base_w)
      updateNumericInput(session, "provisions", value = .mod90_defaults$provisions)
      updateNumericInput(session, "fx_exp",     value = .mod90_defaults$fx_exp)
      updateNumericInput(session, "fx_haircut", value = .mod90_defaults$fx_haircut)
      
      update_currency(.mod90_defaults$currency_sel)
      update_risk_w(.mod90_defaults$risk_weighting_pct / 100)
      
      # 4) Regulatory
      updateSelectInput(session, "asset_rw_method", selected = .mod90_defaults$asset_rw_method)
      updateSelectInput(session, "sec_rw_method",   selected = .mod90_defaults$sec_rw_method)
      updateCheckboxInput(session, "sts",           value = .mod90_defaults$sts)
      updateSelectInput(session, "pool_type",       selected = .mod90_defaults$pool_type)
      updateCheckboxInput(session, "funding_benefit", value = .mod90_defaults$funding_benefit)
      updateRadioButtons(session, "cost_of_capital", selected = .mod90_defaults$cost_of_capital)
      
      # 5) Structure
      updateSelectInput(session, "deal_type", selected = .mod90_defaults$deal_type)
      updateDateInput(session, "effective_date", value = .mod90_defaults$effective_date)
      updateDateInput(session, "final_maturity_date", value = .mod90_defaults$final_maturity_date)
      updateDateInput(session, "time_call_date", value = .mod90_defaults$time_call_date)
      updateSelectInput(session, "amort_type", selected = .mod90_defaults$amort_type)
      updateNumericInput(session, "excess_spread", value = .mod90_defaults$excess_spread)
      updateSelectInput(session, "frequency", selected = .mod90_defaults$frequency)
      
      # 6) Costs
      updateNumericInput(session, "external_upfront", value = .mod90_defaults$external_upfront)
      updateNumericInput(session, "external_running", value = .mod90_defaults$external_running)
      updateNumericInput(session, "tax_group", value = .mod90_defaults$tax_group)
      updateNumericInput(session, "tax_local", value = .mod90_defaults$tax_local)
      
      # 7) Capital components numeric inputs + reset reactive tables
      updateNumericInput(session, "ocf_group", value = .mod90_defaults$ocf_group)
      updateNumericInput(session, "ocf_local", value = .mod90_defaults$ocf_local)
      cap_group(.cap_defaults_group)
      cap_local(.cap_defaults_local)
      
      # 8) Keys inputs
      updateNumericInput(session, "keys_ref_notional", value = .mod90_defaults$keys_ref_notional)
      updateNumericInput(session, "keys_wal", value = .mod90_defaults$keys_wal)
      updateNumericInput(session, "keys_cpr", value = .mod90_defaults$keys_cpr)
      updateNumericInput(session, "keys_n", value = .mod90_defaults$keys_n)
      updateNumericInput(session, "keys_longest_mat_months", value = .mod90_defaults$keys_longest_mat_months)
      
      # 9) Retained positions reset
      rv$retained_positions <- retained_defaults
      
      # 10) SEC-IRBA inputs reset (if present)
      if (exists("sec_irba_inputs_rv") && is.function(sec_irba_inputs_rv)) {
        sec_irba_inputs_rv(sec_irba_default_inputs_df())
      }
      
      # 11) Reset tranche modal internal state
      rv$sold <- NULL
      rv$unfunded <- NULL
      rv$tranche_names <- NULL
      
      # Keep tranche_df Sold.default consistent after reset (if tranche_df exists already)
      df_up <- tryCatch(tranche_df(), error = function(e) NULL)
      if (!is.null(df_up) && is.data.frame(df_up) && nrow(df_up) > 0) {
        init <- initialize_rv_values(df_up)
        rv$sold <- init$sold
        rv$unfunded <- init$unfunded
        if ("Sold.default" %in% names(df_up) && length(rv$sold) == nrow(df_up)) {
          df_up$Sold.default <- rv$sold
          bus("tranche_df", df_up)
        }
      }
      
      # 12) Close tranche modal if open
      manage_tranche_modal("close")
      
      # 13) Reset subtab
      try(updateTabsetPanel(session, "SRT_subtab", selected = .mod90_defaults$SRT_subtab), silent = TRUE)
      rv$last_SRT_subtab <- .mod90_defaults$SRT_subtab
    }
    
    observeEvent(input$resetMod, {
      showModal(
        modalDialog(
          title = "Confirm Reset",
          tagList(
            p("Are you sure you want to reset all inputs?"),
            p(strong("Warning:"), "All current session inputs in this subtab will be lost and cannot be recovered unless previously saved."),
            p("This action cannot be undone.")
          ),
          easyClose = TRUE,
          footer = tagList(
            actionButton(ns("confirmReset"), "Reset", class = "btn-danger"),
            modalButton("Cancel")
          )
        )
      )
    })
    
    observeEvent(input$confirmReset, {
      removeModal()
      reset_mod90_to_defaults()
    })
    
    mod90_data <- reactiveValues()
    
    # --------------------------------------------------------------------
    # NEW: ERBA viewer (static, read-only)
    # - Two hard-coded tables (non-STC and STC) based on the provided CSVs
    # - Monthly data placeholder (static structure only)
    # --------------------------------------------------------------------
    # Hard-code ERBA non-STC table (exact values from provided Non-Stc.csv)
    erba_nonstc_df <- local({
      data.frame(
        Rating = c("AAA","AA+","AA","AA-","A+","A","A-","BBB+","BBB","BBB-","BB+","BB","BB-","B+","B","B-","CCC+/CCC/CCC-","NR"),
        CQS = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,"All other"),
        `Senior 1 year` = c("15%","15%","25%","30%","40%","50%","60%","75%","90%","120%","140%","160%","200%","250%","310%","380%","460%","1250%"),
        `Senior 5 years` = c("20%","30%","40%","45%","50%","65%","70%","90%","105%","140%","160%","180%","225%","280%","340%","420%","505%","1250%"),
        `Non-senior (thin) 1 year` = c("15%","15%","30%","40%","60%","80%","120%","170%","220%","330%","470%","620%","750%","900%","1050%","1130%","1250%","1250%"),
        `Non-senior (thin) 5 years` = c("70%","90%","120%","140%","160%","180%","210%","260%","310%","420%","580%","760%","860%","950%","1050%","1130%","1250%","1250%"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    
    # Hard-code ERBA STC table (exact values from provided STC.xlsx.csv)
    erba_stc_df <- local({
      data.frame(
        Rating = c("AAA","AA+","AA","AA-","A+","A","A-","BBB+","BBB","BBB-","BB+","BB","BB-","B+","B","B-","CCC+/CCC/CCC-","NR"),
        CQS = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,"All other"),
        `Senior 1 year` = c("10%","10%","15%","15%","20%","30%","35%","45%","55%","70%","120%","135%","170%","225%","280%","340%","415%","1250%"),
        `Senior 5 years` = c("10%","15%","20%","25%","30%","40%","40%","55%","65%","85%","135%","155%","195%","250%","305%","380%","455%","1250%"),
        `Non-senior (thin) 1 year` = c("15%","15%","15%","25%","35%","60%","95%","150%","180%","270%","405%","535%","645%","810%","945%","1015%","1250%","1250%"),
        `Non-senior (thin) 5 years` = c("40%","55%","70%","80%","95%","135%","170%","225%","255%","345%","500%","655%","740%","855%","945%","1015%","1250%","1250%"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    
    # in mod90_server(), somewhere near econ_summary_base() / ERBA wiring
    erba_monthly_attachment <- reactive({
      econ_not_df <- econ_notional_base()   # use the existing reactive
      compute_erba_attachment_points(econ_not_df)
    })
    
    
    erba_monthly_detachment <- reactive({
      attach_df <- erba_monthly_attachment()
      compute_erba_detachment_points(attach_df)
    })
    
    erba_monthly_T <- reactive({
      attach_df <- erba_monthly_attachment()
      detach_df <- erba_monthly_detachment()
      compute_erba_T_points(attach_df, detach_df)
    })
    
    # helper: number of WAL / ERBA months = periods x 2
    erba_n_months <- reactive({
      p <- rv$base_periods
      if (is.null(p) || is.na(p)) p <- mosBase
      as.integer(2L * p)
    })
    
    
    erba_monthly_wal <- reactive({
      compute_erba_wal(
        tranche_df_prepared(),
        n_months = erba_n_months()
      )
    })
    
    
    erba_monthly_mt <- reactive({
      wal_df <- erba_monthly_wal()
      compute_erba_mt(wal_df)
    })
    
    erba_monthly_rating <- reactive({
      compute_erba_rating(tranche_df_prepared(), names(erba_monthly_wal())[-1])
    })
    
    erba_monthly_snr1y <- reactive({
      compute_erba_snr_1y(erba_monthly_rating(), erba_nonstc_df)
    })
    
    erba_monthly_snr5y <- reactive({
      compute_erba_snr_5y(erba_monthly_rating(), erba_nonstc_df)
    })
    
    erba_monthly_mezz1y <- reactive({
      compute_erba_mezz_1y(erba_monthly_rating(), erba_nonstc_df)
    })
    
    erba_monthly_mezz5y <- reactive({
      compute_erba_mezz_5y(erba_monthly_rating(), erba_nonstc_df)
    })
    
    
    # … same idea for snr5, mezz1, mezz5 …
    
    erba_monthly_rw_nonsts_base <- reactive({
      compute_erba_rw_nonsts_base(
        T_df     = erba_monthly_T(),
        mt_df    = erba_monthly_mt(),
        rating_df= erba_monthly_rating(),
        snr1_df  = erba_monthly_snr1y(),
        snr5_df  = erba_monthly_snr5y(),
        mezz1_df = erba_monthly_mezz1y(),
        mezz5_df = erba_monthly_mezz5y()
      )
    })
    
    
    
    
    # Render ERBA tables (view-only)
    output$erba_nonstc_tbl <- render_pro_table_erba_risk({
      df <- erba_nonstc_df
      df <- apply_pro_table_ui_erba_riskweights(df)
      df
    })
    
    output$erba_stc_tbl <- render_pro_table_erba_risk({
      df <- erba_stc_df
      df <- apply_pro_table_ui_erba_riskweights(df)
      df
    })
    
    
    
    
    format_erba_pct <- function(df) {
      out <- df
      num_cols <- vapply(out, is.numeric, logical(1))
      # keep first col (Tranche) as plain text
      if ("Tranche" %in% names(out)) num_cols[match("Tranche", names(out))] <- FALSE
      
      for (nm in names(out)[num_cols]) {
        x <- out[[nm]]
        # NA values become plain NA (no "%" sign), like the Snr 1y table
        out[[nm]] <- ifelse(
          is.na(x),
          NA_character_,
          sprintf("%.2f%%", 100 * as.numeric(x))
        )
      }
      out
    }
    
    
    output$erba_monthly_attach_tbl <- render_pro_table_erba({
      df <- erba_monthly_attachment(); req(df)
      df <- erba_trim_and_label_0_99(df)              # NEW
      format_erba_pct(df)
      df <- apply_pro_table_ui_erba_tranche(df, fmt = "pct")
      
      df
    })
    
    output$erba_monthly_detach_tbl <- render_pro_table_erba({
      df <- erba_monthly_detachment(); req(df)
      df <- erba_trim_and_label_0_99(df)              # NEW
      format_erba_pct(df)
      df <- apply_pro_table_ui_erba_tranche(df, fmt = "pct")
      
      df
    })
    
    output$erba_monthly_T_tbl <- render_pro_table_erba({
      df <- erba_monthly_T(); req(df)
      df <- erba_trim_and_label_0_99(df)              # NEW
      format_erba_pct(df)
      df <- apply_pro_table_ui_erba_tranche(df, fmt = "pct")
      
      df
    })
    
    output$erba_monthly_wal_tbl <- render_pro_table_erba({
      df <- erba_monthly_wal(); req(df)
      df <- erba_trim_and_label_0_99(df)              # NEW
      
      df_fmt <- df
      num_cols <- setdiff(names(df_fmt), "Tranche")
      
      # Find the two special rows
      idx_scen  <- which(df_fmt$Tranche == "Scenario index")
      idx_shift <- which(df_fmt$Tranche == "WAL (scheduled)")
      
      for (nm in num_cols) {
        col_num <- as.numeric(df_fmt[[nm]])
        col_chr <- character(length(col_num))
        
        # 1) Scenario index row – whole numbers (no decimals)
        if (length(idx_scen) == 1) {
          col_chr[idx_scen] <- sprintf("%.0f", col_num[idx_scen])
        }
        
        # 2) WAL (scheduled) row – 4 decimal places
        if (length(idx_shift) == 1) {
          col_chr[idx_shift] <- sprintf("%.4f", col_num[idx_shift])
        }
        
        # 3) All tranche rows – 2 decimal places
        other_rows <- setdiff(seq_along(col_num), c(idx_scen, idx_shift))
        col_chr[other_rows] <- sprintf("%.2f", col_num[other_rows])
        
        df_fmt[[nm]] <- col_chr
      }
      df_fmt <- apply_pro_table_ui_erba_tranche(df_fmt)   # fmt = "auto"
      
      df_fmt
    })
    
    output$erba_monthly_mt_tbl <- render_pro_table_erba({
      df <- erba_monthly_mt(); req(df)
      df <- erba_trim_and_label_0_99(df)              # NEW
      
      df_fmt <- df
      num_cols <- setdiff(names(df_fmt), "Tranche")
      
      # MT is just numbers in years, like WAL – 2 decimal places everywhere
      for (nm in num_cols) {
        df_fmt[[nm]] <- sprintf("%.2f", as.numeric(df_fmt[[nm]]))
      }
      df_fmt <- apply_pro_table_ui_erba_tranche(df_fmt)
      df_fmt
    })
    
    output$erba_monthly_rating_tbl <- render_pro_table_erba({
      df <- erba_monthly_rating(); req(df)
      df <- erba_trim_and_label_0_99(df)              # NEW
      # All values are character ratings; just return as-is.
      df <- apply_pro_table_ui_erba_tranche(df)   # fmt = "auto"
      df
    })
    
    
    output$erba_monthly_snr1y_tbl <- render_pro_table_erba({
      df <- erba_monthly_snr1y(); req(df)
      df <- erba_trim_and_label_0_99(df)              # NEW
      
      df_fmt <- df
      num_cols <- setdiff(names(df_fmt), "Tranche")
      
      # Format as percentages (e.g. 15.00%)
      for (nm in num_cols) {
        x <- df_fmt[[nm]]
        df_fmt[[nm]] <- ifelse(
          is.na(x),
          NA_character_,          # mirrors Excel #N/A as NA
          sprintf("%.2f%%", 100 * x)
        )
      }
      
      df_fmt <- apply_pro_table_ui_erba_tranche(df_fmt)   # fmt = "auto"
      df_fmt
      
    })
    
    output$erba_monthly_snr5_tbl <- render_pro_table_erba({
      df <- erba_monthly_snr5y(); req(df)       
      df <- erba_trim_and_label_0_99(df)
      format_erba_pct(df)
      df <- apply_pro_table_ui_erba_tranche(df) 
    })
    
    output$erba_monthly_mezz1_tbl <- render_pro_table_erba({
      df <- erba_monthly_mezz1y(); req(df)      
      df <- erba_trim_and_label_0_99(df)
      format_erba_pct(df)
      df <- apply_pro_table_ui_erba_tranche(df) 
    })
    
    output$erba_monthly_mezz5_tbl <- render_pro_table_erba({
      df <- erba_monthly_mezz5y(); req(df)      
      df <- erba_trim_and_label_0_99(df)
      format_erba_pct(df)
      df <- apply_pro_table_ui_erba_tranche(df) 
    })
    
    output$erba_monthly_rw_nonsts_base_tbl <- render_pro_table_erba({
      df <- erba_monthly_rw_nonsts_base(); req(df)
      df <- erba_trim_and_label_0_99(df)
      
      # ===== DEBUG BLOCK – you can delete later =====
      T_df_raw      <- erba_monthly_T()
      mt_df_raw     <- erba_monthly_mt()
      rating_df_raw <- erba_monthly_rating()
      snr1_df_raw   <- erba_monthly_snr1y()
      snr5_df_raw   <- erba_monthly_snr5y()
      mezz1_df_raw  <- erba_monthly_mezz1y()
      mezz5_df_raw  <- erba_monthly_mezz5y()
      
      # second data column name in RW table
      col_name <- names(df)[3]   # 1 = "Tranche", 2 = Month 0, 3 = Month 1
      
      debug_vec <- c(
        T_B  = T_df_raw[T_df_raw$Tranche == "Class B", col_name],
        T_C  = T_df_raw[T_df_raw$Tranche == "Class C", col_name],
        MT_B = mt_df_raw[mt_df_raw$Tranche == "Class B", col_name],
        MT_C = mt_df_raw[mt_df_raw$Tranche == "Class C", col_name],
        RW_B = df[df$Tranche == "Class B", col_name],
        RW_C = df[df$Tranche == "Class C", col_name]
      )
      print(debug_vec)
      # ===== END DEBUG BLOCK =====
      
      # format as percentages, same as Snr / Mezz tables:
      df_fmt <- df
      
      for (nm in names(df_fmt)) {
        if (nm != "Tranche") {
          x <- df_fmt[[nm]]
          df_fmt[[nm]] <- ifelse(
            is.na(x),
            "<span style='color:#b3b3b3'>NA</span>",
            sprintf("%.2f%%", 100 * as.numeric(x))
          )
        }
      }
      df_fmt <- apply_pro_table_ui_erba_tranche(df_fmt) 
      df_fmt
    })
    
    
    
    
    
    # ------------------------------------------------------------------
    # End ERBA viewer
    # ------------------------------------------------------------------
    
    # -----------------------------------------------------------------
    # UPDATED assign_from_upload() and many other functions continue...
    # (rest of file remains unchanged)
    # -----------------------------------------------------------------
    
    validate_mod90_shape <- validate_mod90_shape # keep reference - already present above
    
    return(list(
      subtab = reactive(input$SRT_subtab)
    ))
    
  })
}


######################################################################################
## Dummy parent App for running the module standalone (with SRT + subtabs)
######################################################################################

options(shiny.dev.mode = TRUE)

ui <- fluidPage(
  titlePanel("Standalone SRT Module"),
  
  tags$head(
    # Ensure this uses the updated professional CSS
    tags$link(rel = "stylesheet", type = "text/css", href = "mod90.css")
  ),
  sidebarLayout(
    # Match width = 4 used in App.R
    sidebarPanel(
      width = 4,
      mod90_sidebar("mod90"),
      mod95_sidebar("mod95")
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "tabselected",
        tabPanel(
          "SRT (mod90)", value = "SRT",
          div(style = "margin-top: 5px"),
          mod90_ui_output("mod90")
        ),
        
        tabPanel(
          "Tranches (mod95)", value = "mod_95",
          div(style = "margin-top: 5px"),
          mod95_ui_output("mod95")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  bus <- bus_SRT()
  mod95_server("mod95", bus = bus)
  mod90_server("mod90", bus = bus)
}

shinyApp(ui, server)