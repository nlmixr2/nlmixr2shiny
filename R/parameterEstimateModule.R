# trackChanges <- function(originalDf, modifiedDf) {
#   if (!all(dim(originalDf) == dim(modifiedDf))) {
#     stop("Data frames must have the same dimensions for comparison.")
#   }
#
#   changedRows <- vapply(seq_len(nrow(originalDf)),
#                         function(i) {
#                           rowChanged <- vapply(seq_along(originalDf[i, ]),
#                                                function(j) {
#                                                original <- originalDf[i,j]
#                                                modified <- modifiedDf[i,j]
#                                                if(is.logical(original) && is.logical(modified)){
#                                                  original!=modified
#                                                } else if(is.character(original)&&is.character(modified)){
#                                                  original!=modified
#                                                } else if(is.numeric(original)&&is.numeric(modified)){
#                                                  abs(original-modified)>1e-4
#                                                } else{
#                                                  FALSE
#                                                }
#                                                }, logical(1))
#                           any(rowChanged)
#                         }, logical(1))
#
#   # Return the rows that have changed
#   modifiedDf[which(changedRows), , drop = FALSE]
# }
#
#
# trackChangesWithinRow <- function(originalDf, modifiedDf) {
#   if (!all(dim(originalDf) == dim(modifiedDf))) {
#     stop("Data frames must have the same dimensions for comparison.")
#   }
#
#   changedDetails <- lapply(seq_len(nrow(originalDf)), function(i) {
#     vapply(seq_along(originalDf[i, ]), function(j) {
#       original <- originalDf[i, j]
#       modified <- modifiedDf[i, j]
#
#       if (is.logical(original) && is.logical(modified)) {
#         original != modified
#       } else if (is.character(original) && is.character(modified)) {
#         original != modified
#       } else if (is.numeric(original) && is.numeric(modified)) {
#         abs(original - modified) > 1e-4
#       } else {
#         FALSE
#       }
#     }, logical(1))
#   })
#
#   # Use the "name" column as the row identifier instead of row numbers
#   if ("name" %in% colnames(originalDf)) {
#     names(changedDetails) <- originalDf$name
#   } else {
#     stop("'name' column not found in the dataframe.")
#   }
#
#   changedDetails
# }



trackChanges <- function(originalDf, modifiedDf) {
  if (!all(dim(originalDf) == dim(modifiedDf))) {
    stop("Data frames must have the same dimensions for comparison.")
  }

  changedRows <- vapply(seq_len(nrow(originalDf)),
                        function(i) {
                          rowChanged <- vapply(seq_along(originalDf[i, ]),
                                               function(j) {
                                                 original <- originalDf[i, j]
                                                 modified <- modifiedDf[i, j]
                                                 if (is.logical(original) && is.logical(modified)) {
                                                   original != modified
                                                 } else if (is.character(original) && is.character(modified)) {
                                                   original != modified
                                                 } else if (is.numeric(original) && is.numeric(modified)) {
                                                   abs(original - modified) > 1e-4
                                                 } else {
                                                   FALSE
                                                 }
                                               }, logical(1))
                          any(rowChanged)
                        }, logical(1))

  # Use the first column as the row identifier instead of row numbers
  changedRowsNames <- originalDf[which(changedRows), 1, drop = FALSE]


  # Return the rows that have changed with the first column value as the identifier
  modifiedDf[which(changedRows), , drop = FALSE]
}

trackChangesWithinRow <- function(originalDf, modifiedDf) {
  if (!all(dim(originalDf) == dim(modifiedDf))) {
    stop("Data frames must have the same dimensions for comparison.")
  }

  changedDetails <- lapply(seq_len(nrow(originalDf)), function(i) {
    vapply(seq_along(originalDf[i, ]), function(j) {
      original <- originalDf[i, j]
      modified <- modifiedDf[i, j]

      if (is.logical(original) && is.logical(modified)) {
        original != modified
      } else if (is.character(original) && is.character(modified)) {
        original != modified
      } else if (is.numeric(original) && is.numeric(modified)) {
        abs(original - modified) > 1e-4
      } else {
        FALSE
      }
    }, logical(1))
  })

  # Use the first column as the row identifier instead of row numbers
  if (!is.null(originalDf[, 1])) {
   names(changedDetails) <- originalDf[,1]
  } else {
    stop("The first column is missing or invalid.")
  }

  changedDetails
}



generateChangeMessages <- function(df, modDF) {
  if (nrow(df) == 0) {
    return(character(0))
  }
  
  paste(vapply(seq_len(nrow(df)), function(i) {
    rowNumber <- df$Row[i]
    pipen <- character(0)
    changedColumns <- df$ChangedColumns[i]
    
    columnList <- unlist(strsplit(as.character(changedColumns), split = ",\\s*"))
    columnString <- paste(columnList, collapse = ", ")
    
    # Handle changes in 'Trans.'
    if ("Trans." %in% columnList) {
      transValue <- modDF$Trans.[rowNumber]
      lhs <- modDF$lhs[rowNumber]
      name <- modDF$name[rowNumber]
      
      if (transValue %in% c("", "Normal", "Untransformed")) {
        pipen <- c(pipen, paste0("model(", lhs, "=", name, ")"))
      } else if (transValue %in% c("exp", "LogNormal")) {
        pipen <- c(pipen, paste0("model(", lhs, "=exp(", name, "))"))
      } else if (transValue %in% c("expit", "LogitNormal")) {
        pipen <- c(pipen, paste0("model(", lhs, "=expit(", name, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], "))"))
      } else if (transValue %in% c("probitInv", "ProbitNormal")) {
        pipen <- c(pipen, paste0("model(", lhs, "=probitInv(", name, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], "))"))
      }
    }
    
    # Handle changes in 'est' or 'Trans.' for Fixed=FALSE
    if (("est" %in% columnList || "Trans." %in% columnList) && modDF$fix[rowNumber] == FALSE) {
      transValue <- modDF$Trans.[rowNumber]
      name <- modDF$name[rowNumber]
      lower <- modDF$lower[rowNumber]
      est <- modDF$est[rowNumber]
      upper <- modDF$upper[rowNumber]
      
      if (transValue %in% c("", "Normal", "Untransformed")) {
        pipen <- c(pipen, paste0("ini(", name, "=c(", lower, ",", est, ",", upper, "))"))
      } else if (transValue %in% c("exp", "LogNormal")) {
        pipen <- c(pipen, paste0("ini(", name, "=c(log(", lower, "), log(", est, "), log(", upper, ")))"))
      } else if (transValue %in% c("expit", "LogitNormal")) {
        pipen <- c(pipen, paste0("ini(", name, "=c(logit(", lower, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], "),",
                                 "logit(", est, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], "),",
                                 "logit(", upper, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], ")))"))
      } else if (transValue %in% c("probitInv", "ProbitNormal")) {
        pipen <- c(pipen, paste0("ini(", name, "=c(probit(", lower, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], "),",
                                 "probit(", est, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], "),",
                                 "probit(", upper, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], ")))"))
      }
    }
    
    # Handle changes in 'est' or 'Trans.' for Fixed=TRUE
    if (("est" %in% columnList || "Trans." %in% columnList) && modDF$fix[rowNumber] == TRUE) {
      transValue <- modDF$Trans.[rowNumber]
      name <- modDF$name[rowNumber]
      lower <- modDF$lower[rowNumber]
      est <- modDF$est[rowNumber]
      upper <- modDF$upper[rowNumber]
      
      if (transValue %in% c("", "Normal", "Untransformed")) {
        pipen <- c(pipen, paste0("ini(", name, "=fix(", lower, ",", est, ",", upper, "))"))
      } else if (transValue %in% c("exp", "LogNormal")) {
        pipen <- c(pipen, paste0("ini(", name, "=fix(log(", lower, "), log(", est, "), log(", upper, ")))"))
      } else if (transValue %in% c("expit", "LogitNormal")) {
        pipen <- c(pipen, paste0("ini(", name, "=fix(logit(", lower, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], "),",
                                 "logit(", est, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], "),",
                                 "logit(", upper, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], ")))"))
      } else if (transValue %in% c("probitInv", "ProbitNormal")) {
        pipen <- c(pipen, paste0("ini(", name, "=fix(probit(", lower, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], "),",
                                 "probit(", est, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], "),",
                                 "probit(", upper, ",", modDF$Trans.Lower[rowNumber], ",", modDF$Trans.Upper[rowNumber], ")))"))
      }
    }
    
    # Handle changes in 'fix' column
    if ("fix" %in% columnList) {
      lhs <- modDF$lhs[rowNumber]
      pipen <- c(pipen, paste0("ini(", lhs, "=fix)"))
    }
    
    if ("Eta" %in% columnList) {
      pipen <- c(pipen, paste0("addEta('", modDF$name[rowNumber], "')"))
    }
    
    pipen <- pipen[pipen != ""]  # Remove empty strings
    paste(pipen, collapse = "|>\n\t")
  }, character(1)), collapse = '|>\n\t')
}







ParEstUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Parameter Estimate"),
    rHandsontableOutput(ns("initalEstimates")),
    h3("Modified Rows"),
    rHandsontableOutput(ns("changedEstimates")),
    h3("Changes within Rows"),
    tableOutput(ns("rowChanges")),
    h3("Change Messages"),
    textOutput(ns("changeMessages"))  # Text output for the generated messages
  )
}

ParEstServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    changedDf <- reactiveVal(NULL)
    rowChanges <- reactiveVal(NULL)
    parEstDF <- reactiveVal(NULL)
    changeMessages <- reactiveVal(NULL)
    
    observeEvent(results$pkpdpipe, {
      req(results$pkpdpipe)
      df <- getRhandsontable(eval(str2lang(results$pkpdpipe))) |>
        transformDF()
      df$Eta <- rep("No Variability", nrow(df))
      
      results$parEst <- df
      parEstDF(df)
      
      output$initalEstimates <- renderRHandsontable({
        rhandsontable(df[!is.na(df$lhs), ]) %>%
          hot_col("Trans.", type = "dropdown", source = c("LogNormal", "LogitNormal", "ProbitNormal", "Normal"), allowInvalid = TRUE) %>%
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
      results$parEst <- df
      
      # Track changed rows
      changedDf(trackChanges(parEstDF(), modifiedDf))
      
      # Track detailed changes within rows
      rowChanges(trackChangesWithinRow(parEstDF(), modifiedDf))
      
      # Generate change messages
      changedRowsDf <- data.frame(
        Row = which(sapply(rowChanges(), any)),
        ChangedColumns = sapply(rowChanges()[which(sapply(rowChanges(), any))], function(cols) {
          paste(names(parEstDF())[which(cols)], collapse = ", ")
        })
      )
      
      # Update change messages using the generateChangeMessages function
      changeMessages(generateChangeMessages(changedRowsDf,modifiedDf))
    })
    
    output$changedEstimates <- renderRHandsontable({
      req(changedDf())
      rhandsontable(changedDf())
    })
    
    output$rowChanges <- renderTable({
      req(rowChanges())
      # Format row changes into a readable format
      changes <- rowChanges()
      changedRows <- which(sapply(changes, any)) # Only display rows that have changes
      
      data.frame(
        Row = changedRows,
        ChangedColumns = sapply(changes[changedRows], function(cols) {
          paste(names(parEstDF())[which(cols)], collapse = ", ") # Use column names instead of numbers
        })
      )
    })
    
    # Render the change messages as text
    output$changeMessages <- renderText({
      req(changeMessages())
      paste(changeMessages(), collapse = "\n")  # Display each message on a new line
    })
    
    return(list(
      changedDf = changedDf,
      rowChanges = rowChanges,
      changeMessages = changeMessages
    ))
  })
}

