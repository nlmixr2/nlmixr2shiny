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
    })

    observeEvent(results$forCov, {
      req(results$forCov)
      editorContent(paste0("model <- ", paste(deparse(as.function(results$forCov)), collapse="\n")))
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
        rstudioapi::viewer("about:blank")  # Close the viewer
      }
    })
  })
}

aceUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(1,  actionButton(ns("copyCode"),
                               ifelse(rstudioapi::isAvailable(),
                                      "Insert & Exit",
                                      "Copy"))),
      column(5, {
        selectInput(ns("theme"), NULL,
                    shinyAce::getAceThemes(),
                    selected="solarized_light",
                    multiple=FALSE)
      }),
      column(6, {
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
