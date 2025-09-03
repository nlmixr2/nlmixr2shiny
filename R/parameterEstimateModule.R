ParEstUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        fluidRow(
          column(
            2,
            actionButton("goPlot", "Update Plots", align = "right"),
            rhandsontable::rHandsontableOutput(ns("plotOptions")),
            h4("Time sampling"),
            rhandsontable::rHandsontableOutput(ns("timeSampling")),
            h4("Dosing"),
            rhandsontable::rHandsontableOutput(ns("dosingTable1")),
            rhandsontable::rHandsontableOutput(ns("dosingTable2")),
          ),
          column(
            10,
            fluidRow(
              h4("Parameters (listed in back-transformed values)"),
              rhandsontable::rHandsontableOutput(ns("initalEstimates")),
              uiOutput("message", placeholder = FALSE),
              uiOutput("plotTabs")
            )
          )
        )
      )
    )
  )
}


ParEstServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    timeSamplingDf <- reactiveVal(NULL)
    dosingTable1Df <- reactiveVal(NULL)
    dosingTable2Df <- reactiveVal(NULL)
    plotOptionsDf <- reactiveVal(NULL)

    iniDf <- reactiveVal(NULL)

    observeEvent(results$parEstim, {
      req(results$parEstim)
      output$initalEstimates <-
        rhandsontable::renderRHandsontable({
          .df <- results$parEstim$iniDf
          .df <- .df[!is.na(.df$ntheta), ]
          .df <- as.data.frame(t(setNames(.df$est, .df$name)))
          if (is.null(iniDf)) {
            iniDf(.df)
          } else if (!identical(names(iniDf()), names(.df))) {
            iniDf(.df)
          }
          iniDf() %>%
            rhandsontable::rhandsontable(rowHeaders = FALSE)
        })

      output$plotOptions <- rhandsontable::renderRHandsontable({
        if (is.null(plotOptionsDf())) {
          plotOptionsDf(data.frame(
            N.Sub=1,
            logy=FALSE
          ))
        }
        rhandsontable::rhandsontable(plotOptionsDf(), rowHeaders = FALSE,
                                     overflow = "visible") |>
          rhandsontable::hot_col("N.Sub", type = "numeric", allowInvalid = FALSE) |>
          rhandsontable::hot_col("logy", type = "checkbox")
      })

      output$dosingTable1 <- rhandsontable::renderRHandsontable({
        .state <- rxode2::rxModelVars(results$parEstim)$state
        if (is.null(dosingTable1Df())) {
          if (length(.state) > 1) {
            dosingTable1Df(data.frame(
              amt = 1,
              rate = NA_real_
            ))
          } else {
            dosingTable1Df(data.frame(
              amt = 1,
              rate = NA_real_,
              cmt = .state[1]
            ))
          }
        } else if (length(.state) > 1 && length(names(dosingTable1Df())) == 2) {
          dosingTable1Df(cbind(dosingTable1Df(), cmt = .state[1]))
        } else if (length(.state) <= 1 && length(names(dosingTable1Df())) == 3) {
          dosingTable1Df(dosingTable1Df()[, -which(names(dosingTable1Df()) == "cmt")])
        }

        .ret <- rhandsontable::rhandsontable(dosingTable1Df(),
                                             rowHeaders = FALSE,
                                             overflow = "visible") |>
          rhandsontable::hot_col("amt", type = "numeric", allowInvalid = FALSE) |>
          rhandsontable::hot_col("rate", type = "numeric", allowInvalid = TRUE)
        if (length(.state) > 1 && any(names(dosingTable1Df()) == "cmt")) {
          .ret <- .ret |>
            rhandsontable::hot_col("cmt",
                                   type = "dropdown",
                                   source = .state,
                                   allowInvalid = FALSE)
        }
        .ret
      })

      output$dosingTable2 <- rhandsontable::renderRHandsontable({
        if (is.null(dosingTable2Df())) {
            dosingTable2Df(data.frame(
              start = 0,
              interval = NA_real_
              ))
        }

        rhandsontable::rhandsontable(dosingTable2Df(), rowHeaders = FALSE,
                                     overflow = "visible") |>
          rhandsontable::hot_col("start", type = "numeric", allowInvalid = FALSE) |>
          rhandsontable::hot_col("interval", type = "numeric", allowInvalid = TRUE)
      })

      output$timeSampling <- rhandsontable::renderRHandsontable({
        if (is.null(timeSamplingDf())) {
          timeSamplingDf(data.frame(
            start = 0,
            end = 100,
            step = 1
          ))
        }
        rhandsontable::rhandsontable(timeSamplingDf(), rowHeaders = FALSE,
                                     overflow = "visible") |>
          rhandsontable::hot_col("start", type = "numeric", allowInvalid = FALSE) |>
          rhandsontable::hot_col("end", type = "numeric", allowInvalid = FALSE) |>
          rhandsontable::hot_col("step", type = "numeric", allowInvalid = FALSE)
      })
    })

  })
}
