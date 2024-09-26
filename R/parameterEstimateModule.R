trackChanges <- function(originalDf, modifiedDf) {
  # Check if data frames have the same dimensions
  if (!all(dim(originalDf) == dim(modifiedDf))) {
    stop("Data frames must have the same dimensions for comparison.")
  }
  
  # Detect rows that have any changes
  changedRows <- vapply(seq_len(nrow(originalDf)), function(i) {
    rowChanged <- vapply(seq_along(originalDf[i, ]), function(j) {
      original <- originalDf[i, j]
      modified <- modifiedDf[i, j]
      
      # Compare logical, character, and numeric values
      if (is.logical(original) && is.logical(modified)) {
        return(original != modified)
      } else if (is.character(original) && is.character(modified)) {
        return(original != modified)
      } else if (is.numeric(original) && is.numeric(modified)) {
        return(abs(original - modified) > 1e-4)
      } else {
        return(FALSE)  # Fallback for non-comparable types
      }
    }, logical(1))
    
    # If any column in the row has changed, mark the row as changed
    any(rowChanged)
  }, logical(1))
  
  # Use the first column as the row identifier (or row numbers)
  changedRowsNames <- originalDf[which(changedRows), 1, drop = FALSE]
  
  # Return the rows that have changed using the first column as the identifier
  modifiedDf[which(changedRows), , drop = FALSE]
}

trackChangesWithinRow <- function(originalDf, modifiedDf) {
  # Check if data frames have the same dimensions
  if (!all(dim(originalDf) == dim(modifiedDf))) {
    stop("Data frames must have the same dimensions for comparison.")
  }
  
  # Detect changes within each row for every column
  changedDetails <- lapply(seq_len(nrow(originalDf)), function(i) {
    vapply(seq_along(originalDf[i, ]), function(j) {
      original <- originalDf[i, j]
      modified <- modifiedDf[i, j]
      
      # Compare logical, character, and numeric values
      if (is.logical(original) && is.logical(modified)) {
        return(original != modified)
      } else if (is.character(original) && is.character(modified)) {
        return(original != modified)
      } else if (is.numeric(original) && is.numeric(modified)) {
        return(abs(original - modified) > 1e-4)
      } else {
        return(FALSE)  # Fallback for non-comparable types
      }
    }, logical(1))
  })
  
  # Set row identifiers as names for the list (use first column of originalDf)
  if (!is.null(originalDf[, 1])) {
    names(changedDetails) <- originalDf[, 1]
  } else {
    stop("The first column is missing or invalid.")
  }
  
  # Return a list of changes for each row with the first column as the row identifier
  changedDetails
}



generateChangeMessages <- function(df, modDF) {
  if (nrow(df) == 0) {
    return(character(0))
  }
  
  tt <- vapply(seq_len(nrow(df)), function(i) {
    rowNumber <- df$Row[i]
    pipen <- character(0)
    changedColumns <- df$ChangedColumns[i]
    
    columnList <- unlist(strsplit(as.character(changedColumns), split = ",\\s*"))
    columnString <- paste(columnList, collapse = ", ")
    
    # Handle changes in 'Trans.'
    if ("Trans." %in% columnList) {
      lhs <- modDF$lhs[rowNumber]
      transValue <- modDF$Trans.[rowNumber]
      name <- modDF$name[rowNumber]
      lower <- ifelse(!is.na(modDF$lower[rowNumber])||modDF$lower[rowNumber]==0, modDF$lower[rowNumber], -Inf)
      est <- modDF$est[rowNumber]
      upper <- ifelse(!is.na(modDF$upper[rowNumber])||modDF$upper[rowNumber]==0, modDF$upper[rowNumber], Inf)
      trUpper <- ifelse(!is.na(modDF$Trans.Upper[rowNumber]), modDF$Trans.Upper[rowNumber], 1)
      trLower <- ifelse(!is.na(modDF$Trans.Lower[rowNumber]), modDF$Trans.Lower[rowNumber], 0)
      
      if (transValue %in% c("", "Normal", "Untransformed")) {
        pipen <- c(pipen, paste0("model(", lhs, "=", name, ")"))
      } else if (transValue %in% c("exp", "LogNormal")) {
        pipen <- c(pipen, paste0("model(", lhs, "=exp(", name, "))"))
      } else if (transValue %in% c("expit", "LogitNormal")) {
        pipen <- c(pipen, paste0("model(", lhs, "=expit(", name, ",", trLower, ",", trUpper, "))"))
      } else if (transValue %in% c("probitInv", "ProbitNormal")) {
        pipen <- c(pipen, paste0("model(", lhs, "=probitInv(", name, ",", trLower, ",", trUpper, "))"))
      }
    }
    
    # Handle changes in 'est' or 'Trans.' for Fixed=FALSE
    if (("est" %in% columnList || "Trans." %in% columnList) && modDF$fix[rowNumber] == FALSE) {
      transValue <- modDF$Trans.[rowNumber]
      name <- modDF$name[rowNumber]
      lower <- ifelse(!is.na(modDF$lower[rowNumber])||modDF$lower[rowNumber]==0, modDF$lower[rowNumber], -Inf)
      est <- modDF$est[rowNumber]
      upper <- ifelse(!is.na(modDF$upper[rowNumber])||modDF$upper[rowNumber]==0, modDF$upper[rowNumber], Inf)
      trUpper <- ifelse(!is.na(modDF$Trans.Upper[rowNumber]), modDF$Trans.Upper[rowNumber], 1)
      trLower <- ifelse(!is.na(modDF$Trans.Lower[rowNumber]), modDF$Trans.Lower[rowNumber], 0)
      
      if (transValue %in% c("", "Normal", "Untransformed")) {
        pipen <- c(pipen, paste0("ini(", name, "=c(", lower, ",", est, ",", upper, "))"))
      } else if (transValue %in% c("LogNormal")) {
        pipen <- c(pipen, paste0("ini(", name, "=c(log(", lower, "), log(", est, "), log(", upper, ")))"))
      } else if (transValue %in% c("LogitNormal")) {
        pipen <- c(pipen, paste0("ini(", name, "=c(logit(", lower, ",", trLower, ",", trUpper, "),",
                                 "logit(", est, ",", trLower, ",", trUpper, "),",
                                 "logit(", upper, ",", trLower, ",", trUpper, ")))"))
      } else if (transValue %in% c("ProbitNormal")) {
        pipen <- c(pipen, paste0("ini(", name, "=c(probit(", lower, ",", trLower, ",", trUpper, "),",
                                 "probit(", est, ",", trLower, ",", trUpper, "),",
                                 "probit(", upper, ",", trLower, ",", trUpper, ")))"))
      }
    }
    
    # Handle changes in 'est' or 'Trans.' for Fixed=TRUE
    if (("est" %in% columnList || "Trans." %in% columnList) && modDF$fix[rowNumber] == TRUE) {
      transValue <- modDF$Trans.[rowNumber]
      name <- modDF$name[rowNumber]
      lower <- ifelse(!is.na(modDF$lower[rowNumber])||modDF$lower[rowNumber]==0, modDF$lower[rowNumber], -Inf)
      est <- modDF$est[rowNumber]
      upper <- ifelse(!is.na(modDF$upper[rowNumber])||modDF$upper[rowNumber]==0, modDF$upper[rowNumber], Inf)
      trUpper <- ifelse(!is.na(modDF$Trans.Upper[rowNumber]), modDF$Trans.Upper[rowNumber], 1)
      trLower <- ifelse(!is.na(modDF$Trans.Lower[rowNumber]), modDF$Trans.Lower[rowNumber], 0)
      
      if (transValue %in% c("", "Normal", "Untransformed")) {
        pipen <- c(pipen, paste0("ini(", name, "=fix(", lower, ",", est, ",", upper, "))"))
      } else if (transValue %in% c("LogNormal")) {
        pipen <- c(pipen, paste0("ini(", name, "=fix(log(", lower, "), log(", est, "), log(", upper, ")))"))
      } else if (transValue %in% c("LogitNormal")) {
        pipen <- c(pipen, paste0("ini(", name, "=fix(logit(", lower, ",", trLower, ",", trUpper, "),",
                                 "logit(", est, ",", trLower, ",", trUpper, "),",
                                 "logit(", upper, ",", trLower, ",", trUpper, ")))"))
      } else if (transValue %in% c("ProbitNormal")) {
        pipen <- c(pipen, paste0("ini(", name, "=fix(probit(", lower, ",", trLower, ",", trUpper, "),",
                                 "probit(", est, ",", trLower, ",",trUpper, "),",
                                 "probit(", upper, ",", trLower, ",", trUpper, ")))"))
      }
    }
    
    # Handle changes in 'fix' column
    if ("fix" %in% columnList) {
      name <- modDF$name[rowNumber]
      pipen <- c(pipen, paste0("ini(", name, "=fix)"))
    }
    
    if ("Eta" %in% columnList) {
      pipen <- c(pipen, paste0("addEta('", modDF$name[rowNumber], "')"))
    }
    
    pipen <- pipen[pipen != ""]  # Remove empty strings
    paste(pipen, collapse = "|>\n\t")
  }, character(1))
  tt <- tt[tt!=""]
  return(paste(tt,collapse="|>\n\t"))
}







# ParEstUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     h3("Parameter Estimate"),
#     rHandsontableOutput(ns("initalEstimates")),
#     h3("Modified Rows"),
#     rHandsontableOutput(ns("changedEstimates")),
#     # h3("Changes within Rows"),
#     # tableOutput(ns("rowChanges")),
#     # h3("Change Messages"),
#     # textOutput(ns("changeMessages")),
#     fluidRow(
#       column(12, actionButton(ns("copy_code"), "Copy Model Code"))
#     )
#   )
# }
# 
# 
# 
# ParEstServer <- function(id, results) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     changedDf <- reactiveVal(NULL)
#     rowChanges <- reactiveVal(NULL)
#     parEstDF <- reactiveVal(NULL)
#     changeMessages <- reactiveVal(NULL)
#     
#     observeEvent(results$parEstim, {
#       req(results$parEstim)
#       df <- getRhandsontable(results$parEstim) |>
#         transformDF()
#       df$Eta <- rep("No Variability", nrow(df))
#       
#       parEstDF(df)
#       
#       output$initalEstimates <- renderRHandsontable({
#         rhandsontable(df[!is.na(df$lhs), ], rowHeaders = FALSE) %>%
#           hot_col("Trans.", type = "dropdown", source = c("LogNormal", "LogitNormal", "ProbitNormal", "Normal"), allowInvalid = TRUE) %>%
#           hot_col("Eta", type = "dropdown", source = c("Between subject variabilities", "No Variability"), allowInvalid = TRUE) %>%
#           hot_col("lower", type = "numeric", allowInvalid = TRUE) %>%
#           hot_col("upper", type = "numeric", allowInvalid = TRUE)
#       })
#     })
#     
#     observeEvent(input$initalEstimates, {
#       req(input$initalEstimates)
#       modifiedDf <- hot_to_r(input$initalEstimates)
#       
#       modifiedDf <- 
#       
#       if (!all(dim(parEstDF()) == dim(modifiedDf))) {
#         modifiedDf <- modifiedDf[1:nrow(parEstDF()), names(parEstDF())]
#       }
#       
#       changedDf(trackChanges(parEstDF(), modifiedDf))
#       rowChanges(trackChangesWithinRow(parEstDF(), modifiedDf))
#       
#       changedRowsDf <- data.frame(
#         Row = which(sapply(rowChanges(), any)),
#         ChangedColumns = sapply(rowChanges()[which(sapply(rowChanges(), any))], function(cols) {
#           paste(names(parEstDF())[which(cols)], collapse = ", ")
#         })
#       )
#       
#       changeMessages(generateChangeMessages(changedRowsDf, modifiedDf))
#       
#       # Store the changeMessages in results$ParEstimates
#       results$ParEstimates <- changeMessages()
#     })
#     
#     output$changedEstimates <- renderRHandsontable({
#       req(changedDf())
#       rhandsontable(changedDf())
#     })
#     
#     # output$rowChanges <- renderTable({
#     #   req(rowChanges())
#     #   changes <- rowChanges()
#     #   changedRows <- which(sapply(changes, any))
#     #   
#     #   data.frame(
#     #     Row = changedRows,
#     #     ChangedColumns = sapply(changes[changedRows], function(cols) {
#     #       paste(names(parEstDF())[which(cols)], collapse = ", ")
#     #     })
#     #   )
#     # })
#     
#     # output$changeMessages <- renderText({
#     #   req(changeMessages())
#     #   paste(changeMessages(), collapse = "\n")
#     # })
#     
#     return(list(
#       changedDf = changedDf,
#       rowChanges = rowChanges,
#       changeMessages = changeMessages
#     ))
#     
#     observeEvent(input$copy_code, {
#       waiter_show(html = tagList(
#         spin_fading_circles(),  # A nice spinning loading indicator
#         h4("Calculating, please wait...")
#       ))
#       model_code <- paste(deparse(as.function(eval(str2lang(paste(c("results$parEstim", results$ParEstimates), collapse = "|>\n\t"))))), collapse="\n")
#       rstudioapi::insertText(model_code)
#       
#       waiter_hide()
#       stopApp()  
#     })
#   })
# }


ParEstUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Parameter Estimate"),
    rHandsontableOutput(ns("initalEstimates")),
    h3("Modified Rows"),
    rHandsontableOutput(ns("changedEstimates")),
    fluidRow(
      column(12, actionButton(ns("copy_code"), "Copy Model Code"))  # Added the copy code button
    )
  )
}


ParEstServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    changedDf <- reactiveVal(NULL)
    rowChanges <- reactiveVal(NULL)
    parEstDF <- reactiveVal(NULL)
    changeMessages <- reactiveVal(NULL)
    
    observeEvent(results$parEstim, {
      req(results$parEstim)
      df <- getRhandsontable(results$parEstim) |>
        transformDF()
      df$Eta <- rep("No Variability", nrow(df))
      
      parEstDF(df)
      
      output$initalEstimates <- renderRHandsontable({
        rhandsontable(df[!is.na(df$lhs), ], rowHeaders = FALSE) %>%
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
      
      changedDf(trackChanges(parEstDF(), modifiedDf))
      rowChanges(trackChangesWithinRow(parEstDF(), modifiedDf))
      
      changedRowsDf <- data.frame(
        Row = which(sapply(rowChanges(), any)),
        ChangedColumns = sapply(rowChanges()[which(sapply(rowChanges(), any))], function(cols) {
          paste(names(parEstDF())[which(cols)], collapse = ", ")
        })
      )
      
      changeMessages(generateChangeMessages(changedRowsDf, modifiedDf))
      
      # Store the changeMessages in results$ParEstimates
      results$ParEstimates <- changeMessages()
    })
    
    output$changedEstimates <- renderRHandsontable({
      req(changedDf())
      rhandsontable(changedDf())
    })
    
    # Copy model code button functionality
    observeEvent(input$copy_code, {
      # Show the waiter spinner while generating the code
      waiter_show(html = tagList(
        spin_fading_circles(),
        h4("Calculating, please wait...")
      ))
      
      # Create the model code from the pipeline of parameter estimates and changes
      model_code <- paste(
        deparse(as.function(eval(str2lang(paste(c("results$parEstim", results$ParEstimates), collapse = " |> \n\t"))))),
        collapse = "\n"
      )
      
      # Paste the model code into the active R script
      rstudioapi::insertText(model_code)
      
      # Hide the spinner and stop the app
      waiter_hide()
      stopApp()
    })
  })
}
