#' UI for the Import Model module
#'
#' @param id Shiny module ID
#' @return A Shiny UI tagList
#' @noRd
importUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
        shinyWidgets::radioGroupButtons(
          inputId = ns("importType"),
          label = "Source",
          choices = c("NONMEM", "Monolix", "campsismod"),
          selected = "NONMEM"
        )
      ),
      column(9,
        # File chooser – shown for NONMEM and Monolix
        conditionalPanel(
          condition = sprintf("input['%s'] != 'campsismod'", ns("importType")),
          shinyFiles::shinyFilesButton(
            id = ns("importFile"),
            label = "Select file",
            title = "Select a model file to import",
            multiple = FALSE
          )
        ),
        # campsismod library picker – shown only for campsismod
        conditionalPanel(
          condition = sprintf("input['%s'] == 'campsismod'", ns("importType")),
          fluidRow(
            column(4,
              selectInput(ns("campsisCategory"), "Category",
                choices = character(0), selectize = FALSE)
            ),
            column(5,
              selectInput(ns("campsisModel"), "Model",
                choices = character(0), selectize = FALSE)
            ),
            column(3,
              br(),
              actionButton(ns("campsisImport"), "Import",
                icon = icon("file-import"), class = "btn-primary")
            )
          )
        )
      )
    ),
    fluidRow(
      column(12,
        uiOutput(ns("importStatusUi"))
      )
    )
  )
}

#' Server for the Import Model module
#'
#' @param id Shiny module ID
#' @param results Shared reactive values list
#' @return Nothing, called for side effects
#' @noRd
importServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Build roots: always include working dir; add package inst dirs if available
    roots <- reactive({
      r <- c(wd = ".")
      if (requireNamespace("nonmem2rx", quietly = TRUE)) {
        inst <- system.file("", package = "nonmem2rx")
        if (nzchar(inst)) r <- c(r, nonmem2rx = inst)
      }
      if (requireNamespace("monolix2rx", quietly = TRUE)) {
        inst <- system.file("", package = "monolix2rx")
        if (nzchar(inst)) r <- c(r, monolix2rx = inst)
      }
      r
    })

    # Populate campsismod category selector on load
    observe({
      cats <- if (requireNamespace("campsismod", quietly = TRUE)) {
        campsismodCategories()
      } else {
        character(0)
      }
      updateSelectInput(session, "campsisCategory", choices = cats,
                        selected = if (length(cats) > 0L) cats[1L] else NULL)
    })

    # Update model selector when category changes
    observeEvent(input$campsisCategory, {
      req(input$campsisCategory)
      mods <- campsismodModelsInCategory(input$campsisCategory)
      updateSelectInput(session, "campsisModel", choices = mods,
                        selected = if (length(mods) > 0L) mods[1L] else NULL)
    })

    # Register file chooser; filter extensions based on selected source type
    observe({
      req(input$importType)
      if (input$importType == "campsismod") return()
      exts <- if (input$importType == "NONMEM") {
        c("ctl", "nmctl", "txt", "lst", "res", "nmlst")
      } else {
        c("mlxtran")
      }
      shinyFiles::shinyFileChoose(
        input, "importFile",
        roots = roots(),
        filetypes = exts,
        session = session
      )
    })

    # React to file selection (NONMEM / Monolix)
    observeEvent(input$importFile, {
      req(!is.integer(input$importFile))  # integer means nothing selected yet
      req(input$importType != "campsismod")
      paths <- shinyFiles::parseFilePaths(roots(), input$importFile)
      req(nrow(paths) > 0)
      path <- as.character(paths$datapath[[1]])

      waiter::waiter_show(html = tagList(
        waiter::spin_fading_circles(),
        h4("Importing model...")
      ))
      on.exit(waiter::waiter_hide())

      mod <- tryCatch({
        if (input$importType == "NONMEM") {
          if (!requireNamespace("nonmem2rx", quietly = TRUE)) {
            stop("Package 'nonmem2rx' is required. Install it with: install.packages('nonmem2rx')")
          }
          nonmem2rx::nonmem2rx(path)
        } else {
          if (!requireNamespace("monolix2rx", quietly = TRUE)) {
            stop("Package 'monolix2rx' is required. Install it with: install.packages('monolix2rx')")
          }
          monolix2rx::monolix2rx(path)
        }
      }, error = function(e) {
        showNotification(
          paste("Import failed:", conditionMessage(e)),
          type = "error", duration = 10
        )
        NULL
      })

      if (!is.null(mod)) {
        results$pkpdm <- mod
        results$parEstim <- mod
        results$modelModified <- TRUE
        results$modelTypeSwitch <- "Current Model"
        showNotification(
          paste0(input$importType, " model imported successfully."),
          type = "message", duration = 5
        )
      }
    })

    # React to campsismod Import button
    observeEvent(input$campsisImport, {
      req(input$campsisCategory, input$campsisModel)

      if (!requireNamespace("campsismod", quietly = TRUE)) {
        showNotification(
          "Package 'campsismod' is required. Install it with: install.packages('campsismod')",
          type = "error", duration = 10
        )
        return()
      }

      waiter::waiter_show(html = tagList(
        waiter::spin_fading_circles(),
        h4("Importing campsismod model...")
      ))
      on.exit(waiter::waiter_hide())

      key <- paste0(input$campsisCategory, "/", input$campsisModel)
      mod <- tryCatch({
        campsis_mod <- campsismodGetModel(key)
        campsismodToRxUi(campsis_mod)
      }, error = function(e) {
        showNotification(
          paste("campsismod import failed:", conditionMessage(e)),
          type = "error", duration = 10
        )
        NULL
      })

      if (!is.null(mod)) {
        results$pkpdm <- mod
        results$parEstim <- mod
        results$modelModified <- TRUE
        results$modelTypeSwitch <- "Current Model"
        showNotification(
          paste0("campsismod model '", key, "' imported successfully."),
          type = "message", duration = 5
        )
      }
    })

    output$importStatusUi <- renderUI({
      if (isTRUE(results$modelModified) && !is.null(results$pkpdm)) {
        tagList(
          h4("Model imported — navigate to other tabs to inspect or estimate."),
          verbatimTextOutput(ns("importedModelPrint"))
        )
      } else {
        p("No model imported yet. Select a source type and choose a file.")
      }
    })

    output$importedModelPrint <- renderPrint({
      req(results$pkpdm)
      print(results$pkpdm)
    })
  })
}
