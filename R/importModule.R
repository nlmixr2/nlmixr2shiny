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
          choices = c("NONMEM", "Monolix"),
          selected = "NONMEM"
        )
      ),
      column(9,
        shinyFiles::shinyFilesButton(
          id = ns("importFile"),
          label = "Select file",
          title = "Select a model file to import",
          multiple = FALSE
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

    # Register file chooser; filter extensions based on selected source type
    observe({
      req(input$importType)
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

    # React to file selection
    observeEvent(input$importFile, {
      req(!is.integer(input$importFile))  # integer means nothing selected yet
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
