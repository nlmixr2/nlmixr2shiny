aceServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shinyAce::aceAutocomplete("ace")

    editorContent <- reactiveVal(NULL)

    observe({
      shinyAce::updateAceEditor(
        session,
        "ace",
        fontSize = input$fontSize,
        theme = input$theme)
      results$ace <- input$ace
    })

    observeEvent(results$parEstim, {
      req(results$parEstim)
      editorContent(paste0("mod1 <- ", paste(deparse(as.function(results$parEstim)), collapse="\n")))
      shinyAce::updateAceEditor(session,
                                editorId="ace",
                                value = editorContent())
    })
    observeEvent(input$copyCode, {
      req(input$ace)  # Ensure input$ace is not NULL
      code <- input$ace
      if (rstudioapi::isAvailable()) {
        rstudioapi::insertText(code)
      } else {
        clipr::write_clip(code)
        showNotification("Code copied to clipboard",
                         type = "message")
      }
      if (rstudioapi::isAvailable()) {
        shiny::stopApp()  # Stop the Shiny app
      }
    })
  })
}

aceUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,  actionButton(ns("copyCode"),
                               ifelse(rstudioapi::isAvailable(),
                                      "Insert & Exit",
                                      "Copy"))),
      column(4, {
        selectInput(ns("theme"), NULL,
                    shinyAce::getAceThemes(),
                    selected="solarized_light",
                    multiple=FALSE)
      }),
      column(4, {
        sliderInput(ns("fontSize"),NULL,min=6,max=24,value=14,step=1)
      })
    ),
    fluidRow(
      shinyAce::aceEditor(
        outputId = ns("ace"),
        mode = "r",
        # to access content of `selectionId` in server.R use `ace_selection`
        # i.e., the outputId is prepended to the selectionId for use
        # with Shiny modules
        # selectionId = "selection",
        value = "",
        placeholder = "Model code will appear here...",
        theme = "solarized_light",
        fontSize = 14,
        autoComplete = "live",
        autoCompleters = "rlang"
      )
    )
  )
}
