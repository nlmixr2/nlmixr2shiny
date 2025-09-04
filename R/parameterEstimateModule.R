#' Update the parEstim object with the estimates from the UI
#'
#' @param results a list that contains the parEstim ui object,
#'   parameterNames and iniDf data frame
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
updateParEstimWithEsts <- function(results) {
  if (is.null(results$parEstim) ||
        is.null(results$iniDf) ||
        is.null(results$paramNames)) {
    return()
  }
  .iniDf <- results$parEstim$iniDf
  .est <- unlist(results$iniDf)
  if (isTRUE(results$backTransform)) {
    .curEval <- results$parEstim$muRefCurEval
    .name <- results$paramNames
    .est <- vapply(seq_along(.est), function(i) {
      .n <- .name[i]
      .w <- which(.curEval$parameter == .n)
      if (length(.w) != 1) {
        return(.est[i])
      }
      .type <- .curEval$curEval[.w]
      .low <-  .curEval$low[.w]
      .hi <-  .curEval$hi[.w]
      if (.type == "exp") {
        log(.est[i])
      } else if (.type == "expit") {
        rxode2::logit(.est[i], .low, .hi)
      } else if (.type == "probitInv") {
        rxode2::probit(.est[i], .low, .hi)
      } else {
        .est[i]
      }
    }, numeric(1), USE.NAMES=FALSE)
  }
  .iniDf[!is.na(.iniDf$ntheta), "est"] <- .est
  mod <- rxode2::rxUiDecompress(results$parEstim)
  mod$iniDf <- .iniDf
  results$pkpdm <- results$parEstim <- rxode2::rxUiCompress(mod)
}
#'  This is the parameter estimates user interface
#'
#' @param id module id
#' @return  shiny ui
#' @noRd
#' @author Matthew L. Fidler and Emmanuel Adewuyi
ParEstUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        fluidRow(
          column(
            2,
            actionButton(ns("goPlot"), "Update Plots", align = "right"),
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
              column(4, h4("Pop. Parameters")),
              column(4, shinyWidgets::awesomeCheckbox(
                inputId = ns("backTransform"),
                label = "Back-transform the estimates",
                value = TRUE)),
              column(4, shinyWidgets::awesomeCheckbox(
                inputId = ns("useLhs"),
                label = "Use model defined parameter",
                value = TRUE)),
              rhandsontable::rHandsontableOutput(ns("initalEstimates")),
              uiOutput(ns("plotTabs"))
            )
          )
        )
      )
    )
  )
}

solveODE <- function(input, output, session, results, id) {
  waiter::waiter_show(html = tagList(
    waiter::spin_fading_circles(),  # A nice spinning loading indicator
    h4("solving system...")
  ))
  updateParEstimWithEsts(results)

  ns <- NS(id)
  d1 <- rhandsontable::hot_to_r(input$dosingTable1)
  d2 <- rhandsontable::hot_to_r(input$dosingTable2)
  st <- rhandsontable::hot_to_r(input$timeSampling)
  po <- rhandsontable::hot_to_r(input$plotOptions)

  nSub <- po$N.Sub
  if (!is.numeric(nSub)) nSub <- 1
  if (is.na(nSub)) nSub <- 1
  nSub <- round(nSub)

  stime <- st$start
  etime <- st$end
  tstep <- st$step

  dose <- d1$amt
  rate <- d1$rate
  if (!is.numeric(rate)) rate <- 0
  if (is.na(rate)) rate <- 0

  into <- d1$cmt
  if (is.null(into)) into <- 1
  if (!is.numeric(into)) into <- 1
  if (is.na(into)) into <- 1

  start <- d2$start
  interval <- d2$interval
  if (!is.numeric(interval)) interval <- 0
  if (is.na(interval)) interval <- 0

  ndoses <- d2$ndoses
  if (!is.numeric(ndoses)) ndoses <- 1
  if (is.na(ndoses)) ndoses <- 1


  et <- rxode2::eventTable() |>
    rxode2::add.sampling(seq(stime, etime, tstep)) %>%
    rxode2::add.dosing(
      dose = dose, start.time = start,
      nbr.doses = ndoses, rate = rate,
      dosing.interval = interval,
      dosing.to = into)

  results$rxsolve <-  rxode2::rxSolve(
    results$parEstim,
    et, nSub=nSub
  )
  waiter::waiter_hide()
}

#'
#'
#'
#' @param id
#' @param results
#' @return
#' @export
#' @author Matthew L. Fidler,  Zufar Mulyukov and Emmanuel Adewuyi
#' @examples
ParEstServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    timeSamplingDf <- reactiveVal(NULL)
    dosingTable1Df <- reactiveVal(NULL)
    dosingTable2Df <- reactiveVal(NULL)
    plotOptionsDf <- reactiveVal(NULL)

    iniDf <- reactiveVal(NULL)

    observeEvent(input$initalEstimates, {
      results$iniDf <- rhandsontable::hot_to_r(input$initalEstimates)
    })

    observeEvent(input$goPlot, {
      solveODE(input, output, session, results, id)
    })

    observeEvent(input$backTransform, {
      req(iniDf())
      req(results$parEstim)
      req(results$paramNames)

      .curEval <- results$parEstim$muRefCurEval
      .cur <- rhandsontable::hot_to_r(input$initalEstimates)
      results$backTransform <- input$backTransform
      if (isTRUE(input$backTransform)) {
        # Change to back-transform
        for (i in seq_along(results$paramNames)) {
          .n <- results$paramNames[i]
          .w <- which(.curEval$parameter == .n)
          if (length(.w) != 1) {
            next
          }
          .type <- .curEval$curEval[.w]
          .low <-  .curEval$low[.w]
          .hi <-  .curEval$hi[.w]
          if (.type == "exp") {
            .cur[[i]] <- exp(.cur[[i]])
          } else if (.type == "expit") {
            .cur[[i]] <- rxode2::expit(.cur[[i]], .low, .hi)
          } else if (.type == "probitInv") {
            .cur[[i]] <- rxode2::probitInv(.cur[[i]], .low, .hi)
          } else {
            next
          }
        }
      } else {
        # Change to un-transformed
        for (i in seq_along(results$paramNames)) {
          .n <- results$paramNames[i]
          .w <- which(.curEval$parameter == .n)
          if (length(.w) != 1) {
            next
          }
          .type <- .curEval$curEval[.w]
          .low <-  .curEval$low[.w]
          .hi <-  .curEval$hi[.w]
          if (.type == "exp") {
            .cur[[i]] <- log(.cur[[i]])
          } else if (.type == "expit") {
            .cur[[i]] <- rxode2::logit(.cur[[i]], .low, .hi)
          } else if (.type == "probitInv") {
            .cur[[i]] <- rxode2::probit(.cur[[i]], .low, .hi)
          } else {
            next
          }
        }
      }
      iniDf(.cur)
    })

    observeEvent(input$useLhs, {
      req(iniDf())
      req(results$parEstim)
      req(results$paramNames)
      .cur <- iniDf()
      if (isTRUE(input$useLhs)) {
        .thetaLhsDf <- results$parEstim$thetaLhsDf
        .name <- vapply(results$paramNames, function(n) {
          .w <- which(.thetaLhsDf$theta == n)
          if (length(.w) != 1) {
            return(n)
          }
          .thetaLhsDf$lhs[.w]
        }, character(1), USE.NAMES=FALSE)
        names(.cur) <- .name
      } else {
        names(.cur) <- results$paramNames
      }
      iniDf(.cur)
    })

    observeEvent(results$parEstim, {
      req(results$parEstim)
      iniDf(NULL)

      output$initalEstimates <-
        rhandsontable::renderRHandsontable({
          .df <- results$parEstim$iniDf
          .df <- .df[!is.na(.df$ntheta), ]
          if (is.null(iniDf()) ||
                !identical(results$paramNames, .df$name)) {
            .name <- .df$name
            .est <- .df$est
            .curEval <- results$parEstim$muRefCurEval
            .thetaLhsDf <- results$parEstim$thetaLhsDf
            .resetIniDf <- FALSE

            if (isTRUE(input$backTransform)) {
              .est <- vapply(seq_along(.est), function(i) {
                .n <- .name[i]
                .w <- which(.curEval$parameter == .n)
                if (length(.w) != 1) {
                  return(.est[i])
                }
                .type <- .curEval$curEval[.w]
                .low <-  .curEval$low[.w]
                .hi <-  .curEval$hi[.w]
                if (.type == "exp") {
                  exp(.est[i])
                } else if (.type == "expit") {
                  rxode2::expit(.est[i], .low, .hi)
                } else if (.type == "probitInv") {
                  rxode2::probitInv(.est[i], .low, .hi)
                } else {
                  .est[i]
                }
              }, numeric(1), USE.NAMES=FALSE)
            }
            .name0 <- results$paramNames <- .name
            if (isTRUE(input$useLhs)) {
              .name <- vapply(.name, function(n) {
                .w <- which(.thetaLhsDf$theta == n)
                if (length(.w) != 1) {
                  return(n)
                }
                .thetaLhsDf$lhs[.w]
              }, character(1), USE.NAMES=FALSE)
            }
            .df <- as.data.frame(t(setNames(.est, .name)))
            iniDf(.df)
          }
          results$backTransform <- input$backTransform
          iniDf() |>
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
              "start" = 0,
              "interval" = NA_real_,
              "ndoses"= NA_real_
              ))
        }

        rhandsontable::rhandsontable(dosingTable2Df(), rowHeaders = FALSE,
                                     overflow = "visible") |>
          rhandsontable::hot_col("start", type = "numeric", allowInvalid = FALSE) |>
          rhandsontable::hot_col("interval", type = "numeric", allowInvalid = TRUE) |>
          rhandsontable::hot_col("ndoses", type = "numeric", allowInvalid = TRUE)
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
                                     overflow = "visible",
                                     selectCallback=TRUE) |>
          rhandsontable::hot_col("start", type = "numeric", allowInvalid = FALSE) |>
          rhandsontable::hot_col("end", type = "numeric", allowInvalid = FALSE) |>
          rhandsontable::hot_col("step", type = "numeric", allowInvalid = FALSE)
      })

    })

    observeEvent(results$rxsolve, {
      output$plotTabs <- renderUI({
        if (is.null(results$rxsolve) ||
              is.null(results$parEstim)) {
          return(tabsetPanel())
        }
        vars <- colnames(results$rxsolve)
        vars <- vars[!vars %in% c("time", "id", "sim.id", "evid", "cmt", "amt", "rate", "ii", "addl", "ss", "dur", "tad", "mdv", "resetno")]
        v1 <- results$parEstim$predDf$var[1]
        if (!(v1 %in% vars)) v1 <- vars[1]
        if (is.null(input$plotTabs)) {
          selTab <- v1
        } else if (input$plotTabs %in% vars) {
          selTab <- input$plotTabs
        } else {
          selTab <- v1
        }
        tabs <-
          lapply(vars, function(v) {
            plotname0 <- paste0("plot", v)
            plotname <- ns(plotname0)
            output[[plotname0]] <- renderPlot({
              p <- try(eval(bquote(plot(results$rxsolve, .(str2lang(v))))))
              if (inherits(p,"try-error")) {
                plot.new()
                text(0.5, 0.5, paste("Error in plotting", v, ":", p))
              } else {
                if (isTRUE(plotOptionsDf()$logy)) {
                  p <- p + xgxr::xgx_scale_y_log10()
                }
                p
              }
            })
            tabPanel(v, plotOutput(plotname))
          })
        do.call(tabsetPanel, c(tabs, id = ns("plotTabs"), selected = selTab))
      })
    })

  })
}
