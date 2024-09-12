trackChanges <- function(originalDf, modifiedDf) {
  if (!all(dim(originalDf) == dim(modifiedDf))) {
    stop("Data frames must have the same dimensions for comparison.")
  }

  changedRows <- vapply(seq_len(nrow(originalDf)),
                        function(i) {
                          rowChanged <- vapply(seq_along(originalDf[i, ]),
                                               function(j) {
                                               original <- originalDf[i,j]
                                               modified <- modifiedDf[i,j]
                                               if(is.logical(original) && is.logical(modified)){
                                                 original!=modified
                                               } else if(is.character(original)&&is.character(modified)){
                                                 original!=modified
                                               } else if(is.numeric(original)&&is.numeric(modified)){
                                                 abs(original-modified)>1e-4
                                               } else{
                                                 FALSE
                                               }
                                               }, logical(1))
                          any(rowChanged)
                        }, logical(1))

  # Return the rows that have changed
  modifiedDf[which(changedRows), , drop = FALSE]
}


ParEstUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Parameter Estimate"),
    rHandsontableOutput(ns("initalEstimates")),
    h3("Modified Rows"),
    rHandsontableOutput(ns("changedEstimates"))
  )
}

ParEstServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    changedDf <- reactiveVal(NULL)
    parEstDF <- reactiveVal(NULL)

    observeEvent(results$pkpdpipe, {
      req(results$pkpdpipe)
      df <- getRhandsontable(eval(str2lang(results$pkpdpipe))) |>
        transformDF()
      df$Eta <- rep("No Variability", nrow(df))
      results$parEst <- df
      parEstDF(df)

      output$initalEstimates <- renderRHandsontable({
        rhandsontable(df[!is.na(df$lhs), ]) %>%
          hot_col("Trans.", type = "dropdown", source = c("exp", "expit", "probitInv", ""), allowInvalid = TRUE) %>%
          hot_col("Eta", type = "dropdown", source = c("Between subject variabilities", "No Variability"), allowInvalid = TRUE) %>%
          hot_col("lower", type = "numeric", allowInvalid = TRUE) %>%
          hot_col("upper", type = "numeric", allowInvalid = TRUE)
      })
    })

    observeEvent(input$initalEstimates, {
      req(input$initalEstimates)
      modifiedDf <- hot_to_r(input$initalEstimates)

      if (!all(dim(parEstDF()) == dim(modifiedDf))) {
        modifiedDf <- modifiedDf[1:nrow(parEstDF()), names(parEstDF())]
      }

      changedDf(trackChanges(parEstDF(), modifiedDf))
    })

    output$changedEstimates <- renderRHandsontable({
      req(changedDf())
      rhandsontable(changedDf())
    })

    return(list(
      changedDf = changedDf
    ))
  })
}


































# trackChanges <- function(originalDf, modifiedDf) {
#   if (!all(dim(originalDf) == dim(modifiedDf))) {
#     stop("Data frames must have the same dimensions for comparison.")
#   }
#   changedRows <- vapply(seq_len(nrow(originalDf)),
#                         function(i){
#                           #.w <- which(originalDf[i,]!=modifiedDf[i,])
#                           .w <- vapply(seq_along(originalDf[i,]),
#                                        function(j){
#                                          !isTRUE(all.equal(originalDf[i,j],modifiedDf[i,j],
#                                                            tolerance = 1e-4))
#                                        },logical(1))
#                           .w <- which(.w)
#                           if(length(.w)==0){
#                             return(FALSE)
#                           }
#                           print(names(originalDf)[.w])
#                           TRUE
#                         },logical(1))
#
#   modifiedDf[which(changedRows), , drop = FALSE]
# }
#
#
#
# ParEstUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     h3("Parameter Estimate"),
#     rHandsontableOutput(ns("initalEstimates")),
#     h3("Modified Rows"),
#     rHandsontableOutput(ns("changedEstimates"))
#   )
# }
#
#
#
#
# ParEstServer <- function(id, results) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     changedDf <- reactiveVal(NULL)
#     parEstDF <- reactiveVal(NULL)
#
#     observeEvent(results$pkpdpipe, {
#       req(results$pkpdpipe)
#       df <- getRhandsontable(eval(str2lang(results$pkpdpipe)))|>
#         transformDF()
#       df$Eta <- rep("No Variability", nrow(df))
#       results$parEst <- df
#       parEstDF(df)
#
#       output$initalEstimates <- renderRHandsontable({
#         rhandsontable(df[!is.na(df$lhs), ]) %>%
#           hot_col("Trans.", type = "dropdown", source = c("exp", "expit", "probitInv", ""), allowInvalid = TRUE) %>%
#           hot_col("Eta", type = "dropdown", source = c("Between subject variabilities", "No Variability"), allowInvalid = TRUE) %>%
#           hot_col("lower", type = "numeric", allowInvalid = TRUE, validator = "function(value, callback) { callback(true); }") %>%
#           hot_col("upper", type = "numeric", allowInvalid = TRUE, validator = "function(value, callback) { callback(true); }")
#       })
#     })
#
#     observeEvent(input$initalEstimates, {
#       req(input$initalEstimates)
#
#       modifiedDf <- hot_to_r(input$initalEstimates)
#       if (!all(dim(parEstDF()) == dim(modifiedDf))) {
#         modifiedDf <- modifiedDf[1:nrow(parEstDF()), names(parEstDF())]
#       }
#
#       changedDf(trackChanges(parEstDF(), modifiedDf))
#     })
#
#     output$changedEstimates <- renderRHandsontable({
#       req(changedDf())
#       rhandsontable(changedDf())
#     })
#
#         return(list(
#       changedDf = changedDf
#     ))
#   })
# }
































# # trackChanges function as previously defined
# trackChanges <- function(original_df, modified_df) {
#   changes <- original_df != modified_df
#   changed_rows <- apply(changes, 1, any)
#   modified_df[changed_rows, ]
# }
#
#
#
#
# ParEstUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     h3("Parameter Estimate"),
#     rHandsontableOutput(ns("initalEstimates"))
#
#   )
# }
#
# ParEstServer <- function(id, results) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     changed_df <- reactiveVal(NULL)
#     parEstDF <- reactiveVal(NULL)
#
#     observeEvent(results$pkpdpipe, {
#       req(results$pkpdpipe)
#       df <- getRhandsontable(eval(str2lang(results$pkpdpipe)))
#       df$Eta <- rep("No Variability", nrow(df))
#       results$parEst <- df
#       parEstDF(df)
#
#       output$initalEstimates <- renderRHandsontable({
#         rhandsontable(df[!is.na(df$lhs), ]) %>%
#           hot_col("Trans.", type = "dropdown", source = c("exp", "expit", "probitInv", ""), allowInvalid = TRUE) %>%
#           hot_col("Eta",type="dropdown", source = c("Between subject varaibilites","No Variabilty"),allowInvalid = TRUE)
#       })
#     })
#
#     observeEvent(input$initalEstimates, {
#       req(input$initalEstimates)
#       modified_df <- hot_to_r(input$initalEstimates)
#       changed_df(trackChanges(parEstDF(), modified_df))
#     })
#     return(list(
#       changed_df
#     ))
#   })
# }




















# ParEstUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     h3("Parameter Estimate"),
#     rHandsontableOutput(ns("initalEstimates")),  # First rhandsontable
#     #rHandsontableOutput(ns("transTable"))        # Second rhandsontable
#   )
# }
#
#
#
# ParEstServer <- function(id, results) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     changed_df <- reactiveVal(NULL)
#     observeEvent(results$pkpdpipe, {
#       req(results$pkpdpipe)
#       parEstDF <- getRhandsontable(eval(str2lang(results$pkpdpipe)))
#       parEstDF$Eta <- rep(FALSE,nrow(parEstDF))
#       results$parEst <- parEstDF
#       output$initalEstimates <- renderRHandsontable({
#         rhandsontable(parEstDF[!is.na(parEstDF$lhs), ]) %>%
#           hot_col("Trans.", type = "dropdown", source = c("exp", "expit", "probitInv",""),allowInvalid = TRUE)
#       })
#
#
#       observeEvent(output$initalEstimates, {
#         if (!is.null(output$initalEstimates)) {
#           modified_df <- hot_to_r(output$initalEstimates)
#           changed_df(trackChanges(parEstDF, modified_df))
#         }
#       })
#       # output$transTable <- renderRHandsontable({
#       #   rhandsontable(parEstDF)
#       #})
#     })
#   })
# }









# ParEstServer <- function(id, results) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#
#     observeEvent(results$pkpdpipe, {
#       req(results$pkpdpipe)
#       parEstDF <- getRhandsontable(eval(str2lang(results$pkpdpipe)))
#       results$parEst <- parEstDF
#
#       output$initalEstimates <- renderRHandsontable({
#         rhandsontable(parEstDF[, c("name", "Trans.", "Trans.Lower", "Trans.Upper", "lower", "label", "lhs")])
#       })
#
#       output$transTable <- renderRHandsontable({
#         rhandsontable(parEstDF[, c("lower", "est", "upper", "fix", "label", "lhs")])
#       })
#     })
#
#     observeEvent(input$initalEstimates, {
#       editedTable <- hot_to_r(input$initalEstimates)
#       changes <- editedTable != results$parEst
#       changedIndices <- which(changes, arr.ind = TRUE)
#       changedValues <- editedTable[changes]
#
#       print(data.frame(changedIndices, changedValues))
#     })
#   })
# }

