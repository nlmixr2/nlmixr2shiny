addprop <- function(property=c("initial value", "bioavailability", "rate", "duration", "lag time"),
                    compartment){
  property <- match.arg(property)
  switch(property, "initial value"=paste("addIni(",compartment,")"),
         "bioavailability"=paste("addBioavailability(",compartment,")"),
         "rate"=paste("addRate(",compartment,")"),
         "duration"=paste("addDur(",compartment,")"),
         "lag time"=paste("addLag(",compartment,")"))
}


pipeAllProp <- function(df){
  checkmate::assertDataFrame(df)
  if(nrow(df)==0){
    character(0)
  } else {
    vapply(1:nrow(df), function(i){
      addprop(df$Property[i],df$Compartment[i])},character(1))
  }
}


pkprUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    titlePanel("nlmixr2shiny"),
    mainPanel(
      fluidRow(
        column(1, actionButton(ns("add_row"), label = HTML("&nbsp;"), icon = icon("plus")),
               style="column-width:150px; vertical-align: middle; align: center"),
        column(6, uiOutput(ns("compartment_ui")),
               style="column-width:55%; vertical-align: middle; align: center"),
        column(5, uiOutput(ns("property_ui")),
               style="column-width:45%; vertical-align: middle; align: center")
      ),
      fluidRow(column(12,
                      DTOutput(ns("table_output"))
      )),
      fluidRow(
        column(12, HTML("&nbsp;"))
      ),
      fluidRow(
        column(12, textOutput(ns("pipe")))
      )
    )
  )
}



pkprServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      if (!is.null(results$pkpdpipe) && nzchar(results$pkpdpipe)) {
        results$pkpdm <- eval(str2lang(results$pkpdpipe))
      } else {
        results$pkpdm <- list(state = character(0))
      }
    })

    table_data <- reactiveVal(data.frame(Compartment = character(0), Property = character(0), stringsAsFactors = FALSE))

    updatePipeOutput <- function() {
      results$modProp <- pipeAllProp(table_data())
    }

    output$compartment_ui <- renderUI({
      req(results$pkpdm)  # Ensure pkpdm is available
      selectInput(ns("compartment"), "Compartment", choices = results$pkpdm$state, width = "300px", selectize = FALSE, size = 5)
    })

    output$property_ui <- renderUI({
      selectInput(ns("property"), "Property", choices = c("initial value", "bioavailability", "rate", "duration", "lag time"), width = "300px",
                  selectize = FALSE, size = 5)
    })

    observeEvent(input$add_row, {
      new_row <- data.frame(Compartment = input$compartment, Property = input$property, stringsAsFactors = FALSE)
      if (nrow(subset(table_data(), Compartment == new_row$Compartment & Property == new_row$Property)) > 0) {
        showModal(modalDialog(
          title = "Error",
          "This Compartment-Property combination already exists.",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        table_data(rbind(table_data(), new_row))
        updatePipeOutput()  # Update pipeline output when a row is added
      }
    })

    output$table_output <- renderDT({
      td <- table_data()
      if (inherits(td, "data.frame") && nrow(td) > 0) {
        td <- cbind(td, Remove = sprintf('<button class="btn btn-danger btn-sm delete" id="%s">-</button>', 1:nrow(td)))
      } else {
        td <- data.frame()
      }
      w <- which(!(td$Compartment %in% results$pkpdm$state))
      if(length(w) > 0){
        showModal(modalDialog(
          title = "Warning",
          "Dropping compartment properties no longer in the model!",
          easyClose = TRUE,
          footer = NULL
        ))
        td <- td[-w,,drop=FALSE]
        if(nrow(td) == 0){
          td <- data.frame()
        }
      }
      datatable(td, escape = FALSE, selection = 'none', rownames = FALSE,
                options = list(dom = 't', ordering = FALSE, paging = FALSE))
    })

    output$pipe <- renderText({
      paste(c(results$pkpdpipe, results$modProp), collapse = "|>\n\t")
    })

    observeEvent(input$table_output_cell_clicked, {
      info <- input$table_output_cell_clicked
      if (!is.null(info$value) && grepl("delete", info$value)) {
        row_id <- as.numeric(gsub("\\D", "", info$value))
        table_data(table_data()[-row_id, ])
        updatePipeOutput()
      }
    })
  })
}




















# pkprServer <- function(id, results) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#
#     table_data <- reactiveVal(data.frame(Compartment = character(0), Property = character(0), stringsAsFactors = FALSE))
#
#     output$compartment_ui <- renderUI({
#       selectInput(ns("compartment"), "Compartment", choices =results$pkpdm$state, width = "300px", selectize = FALSE, size = 5) #use this later pkModel()$state
#     })
#     output$property_ui <- renderUI({
#       selectInput(ns("property"), "Property", choices = c("initial value", "bioavailability", "rate", "duration", "lag time"), width = "300px",
#                   selectize = FALSE, size=5)
#     })
#     observeEvent(input$add_row, {
#       new_row <- data.frame(Compartment = input$compartment, Property = input$property, stringsAsFactors = FALSE)
#       if (nrow(subset(table_data(), Compartment == new_row$Compartment & Property == new_row$Property)) > 0) {
#         showModal(modalDialog(
#           title = "Error",
#           "This Compartment-Property combination already exists.",
#           easyClose = TRUE,
#           footer = NULL
#         ))
#       } else {
#         table_data(rbind(table_data(), new_row))
#         results$pkpdpipe <- c(results$pkpdpipe,pipeAllProp(table_data()))
#
#       }
#     })
#     output$table_output <- renderDT({
#       td <- table_data()
#       if (inherits(td, "data.frame") && nrow(td) > 0) {
#         td <- cbind(table_data(), Remove = sprintf('<button class="btn btn-danger btn-sm delete" id="%s">-</button>', 1:nrow(table_data())))
#       } else {
#         td <- data.frame()
#       }
#       datatable(td,
#                 escape = FALSE, selection = 'none', rownames = FALSE,
#                 options = list(dom = 't', ordering = FALSE, paging = FALSE)
#       )
#
#     })
#     output$pipe <- renderText({
#       paste(results$pkpdpipe, collapse="|>\n\t")
#     })
#     observeEvent(input$table_output_cell_clicked, {
#       info <- input$table_output_cell_clicked
#       if (!is.null(info$value) && grepl("delete", info$value)) {
#         row_id <- as.numeric(gsub("\\D", "", info$value))
#         table_data(table_data()[-row_id, ])
#         pipVec <- pipeAllProp(table_data())
#
#       }
#     })
#   })
# }
