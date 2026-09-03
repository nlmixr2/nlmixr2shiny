#' Return a named character vector of all campsismod model_suite entries
#'
#' @return Named character vector where both names and values are "category/model"
#'   keys suitable for use in a selectInput. Returns `character(0)` if campsismod
#'   is not installed.
#' @noRd
campsismodModelNames <- function() {
  if (!requireNamespace("campsismod", quietly = TRUE)) return(character(0))
  suite <- campsismod::model_suite
  keys <- unlist(lapply(names(suite), function(cat) {
    nms <- names(suite[[cat]])
    if (length(nms) == 0L) return(character(0))
    stats::setNames(paste0(cat, "/", nms), paste0(cat, "/", nms))
  }))
  keys
}

#' Return the categories present in campsismod::model_suite
#'
#' @return Character vector of category names, or `character(0)` if campsismod
#'   is not installed.
#' @noRd
campsismodCategories <- function() {
  if (!requireNamespace("campsismod", quietly = TRUE)) return(character(0))
  names(campsismod::model_suite)
}

#' Return model names within a given category of campsismod::model_suite
#'
#' @param category Character(1) category name (e.g. `"pk"`).
#' @return Character vector of model names, or `character(0)` if the category
#'   does not exist or campsismod is not installed.
#' @noRd
campsismodModelsInCategory <- function(category) {
  if (!requireNamespace("campsismod", quietly = TRUE)) return(character(0))
  suite <- campsismod::model_suite
  if (!category %in% names(suite)) return(character(0))
  names(suite[[category]])
}

#' Retrieve a single campsismod model by category/name key
#'
#' @param key Character(1) in the form `"category/model_name"`.
#' @return A `campsis_model` S4 object.
#' @noRd
campsismodGetModel <- function(key) {
  parts <- strsplit(key, "/", fixed = TRUE)[[1]]
  campsismod::model_suite[[parts[1]]][[parts[2]]]
}

# ---------------------------------------------------------------------------
# Internal helpers for building ini({}) block text
# ---------------------------------------------------------------------------

#' Build theta lines for the ini block from rxode_params
#'
#' @param model A `campsis_model` object.
#' @return Character(1) – multi-line string of `  THETA_name <- value` entries.
#' @noRd
.campsismodThetaIni <- function(model) {
  params <- campsismod::rxode_params(model)
  if (length(params) == 0L) return("")
  lines <- vapply(names(params), function(nm) {
    sprintf("    %s <- %g", nm, params[[nm]])
  }, character(1L))
  paste(lines, collapse = "\n")
}

#' Build omega lines for the ini block from rxode_matrix("omega")
#'
#' Diagonal matrices produce individual `eta ~ var` entries; full matrices
#' produce a single `eta1 + eta2 + ... ~ c(lower_triangle)` entry.
#'
#' @param model A `campsis_model` object.
#' @return Character(1) – multi-line ini-block omega text, or `""` if no omegas.
#' @noRd
.campsismodOmegaIni <- function(model) {
  omega <- campsismod::rxode_matrix(model, type = "omega")
  n <- nrow(omega)
  if (n == 0L) return("")
  nms <- rownames(omega)

  if (n == 1L) {
    return(sprintf("    %s ~ %g", nms[1L], omega[1L, 1L]))
  }

  off_diag <- omega[lower.tri(omega)]
  if (all(off_diag == 0)) {
    # Diagonal – write each eta separately
    lines <- vapply(seq_len(n), function(i) {
      sprintf("    %s ~ %g", nms[i], omega[i, i])
    }, character(1L))
    return(paste(lines, collapse = "\n"))
  }

  # Non-diagonal – write as a single lotri-style block
  vals <- omega[lower.tri(omega, diag = TRUE)]
  paste0(
    "    ", paste(nms, collapse = " + "), " ~ c(\n",
    "      ", paste(format(vals, digits = 7L), collapse = ", "), "\n",
    "    )"
  )
}

# ---------------------------------------------------------------------------
# Main conversion function
# ---------------------------------------------------------------------------

#' Convert a campsismod model to an rxode2 rxUi object
#'
#' Builds an nlmixr2/rxode2-compatible model from:
#' \itemize{
#'   \item `campsismod::rxode_code()` for the model block (error lines stripped)
#'   \item `campsismod::rxode_params()` for theta initial estimates
#'   \item `campsismod::rxode_matrix(type = "omega")` for the omega block
#' }
#' The sigma matrix is stored as `rxui$mv0$sigma` (metadata) and is intentionally
#' excluded from `iniDf` so the user can configure the residual error model
#' interactively in the Statistical Model tab.
#'
#' @param model A `campsis_model` S4 object (element of
#'   `campsismod::model_suite`).
#' @return An `rxUi` object (result of `rxode2::rxode2()`), with the sigma
#'   matrix attached as `rxui$mv0$sigma`.
#' @noRd
campsismodToRxUi <- function(model) {
  # -- 1. Code lines -----------------------------------------------------------
  code_lines <- campsismod::rxode_code(model)

  # -- 2. Identify sigma EPS variable names ------------------------------------
  all_params <- model@parameters@list
  sigmas <- Filter(function(x) methods::is(x, "sigma"), all_params)
  eps_names <- paste0("EPS_", vapply(sigmas, function(s) s@name, character(1L)))

  # -- 3. Separate error lines from ODE/model lines ----------------------------
  has_eps <- if (length(eps_names) > 0L) {
    vapply(code_lines, function(line) {
      any(vapply(eps_names, function(e) grepl(e, line, fixed = TRUE), logical(1L)))
    }, logical(1L))
  } else {
    rep(FALSE, length(code_lines))
  }

  # Keep non-empty, non-error lines
  model_lines <- code_lines[!has_eps & nzchar(trimws(code_lines))]

  # -- 4. Build ini block text -------------------------------------------------
  theta_ini <- .campsismodThetaIni(model)
  omega_ini <- .campsismodOmegaIni(model)

  ini_body <- paste(
    c(theta_ini, omega_ini)[nzchar(c(theta_ini, omega_ini))],
    collapse = "\n"
  )

  # -- 5. Assemble model function text -----------------------------------------
  func_text <- paste0(
    "function() {\n",
    "  ini({\n",
    ini_body, "\n",
    "  })\n",
    "  model({\n",
    paste0("    ", model_lines, collapse = "\n"), "\n",
    "  })\n",
    "}"
  )

  # -- 6. Parse to rxUi --------------------------------------------------------
  mod_func <- eval(parse(text = func_text))
  rxui <- rxode2::rxode2(mod_func)

  # -- 7. Attach sigma matrix into rxui$meta (the designated metadata env) ----
  sigma_mat <- tryCatch(
    campsismod::rxode_matrix(model, type = "sigma"),
    error = function(e) NULL
  )
  if (!is.null(sigma_mat) && nrow(sigma_mat) > 0L) {
    # rxui$meta is an environment (reference semantics); assign into it directly
    # to avoid the $<-.rxUi write-back that would trigger a "fixed component" error.
    .meta <- rxui$meta
    .meta$sigma <- sigma_mat
  }

  rxui
}
