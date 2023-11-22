#' @name runInvoice
#' @title Function runInvoice
#' @description runInvoice: run shinyApp
#' @param installAll boolean, when \code{TRUE} dependencies are installed without asking. Defaults to \code{FALSE}
#' @keywords shiny
#' @export
#' @rdname runInvoice
#' @importFrom utils install.packages
#' @return shiny
runInvoice <- function(installAll = FALSE) { # nolint
  appDir <- system.file("shinyApps", "invoice_app", package = "shinyInvoice")

  if (appDir == "") {
    stop("Could not find inst folder with shiny app.", call. = FALSE)
  }

  missRentrez <- missRecPkg <- missPkg <- character()

  neededPkg <- c(
    "shiny",
    "dplyr",
    "tibble",
    "rlang",
    "stringr",
    "quantmod",
    "rmarkdown",
    "rhino",
    "tinytex",
    "pillar",
    "rjson",
    "lubridate",
    "rmarkdown",
    "readr",
    "dplyr",
    "quantmod",
    "shinyAce",
    "fs",
    "jsonlite"
  )

  for (pkg in neededPkg) {
    if (system.file(package = pkg) == "") {
      missPkg <- c(missPkg, pkg)
    }
  }

  if (length(missPkg) && installAll == FALSE) {
    message(paste("you need to install", paste(missPkg, collapse = ", ")))
    answer <- readline("Do you want to proceed installing now (Yes or No) ? ")

    if (exists("answer")) {
      if (tolower(answer) %in% c("y", "ye", "yes", "yap", "yess")) {
        lapply(neededPkg, function(pkg) {
          if (system.file(package = pkg) == "") {
            tryCatch(utils::install.packages(pkg), error = function(w) {
              message(paste("failure installing", pkg))
            })
          }
        })
      } else {
        return(print("bye"))
      }
    }
  } else if (length(missPkg) && installAll) {
    lapply(neededPkg, function(pkg) {
      if (system.file(package = pkg) == "") {
        tryCatch(utils::install.packages(pkg), error = function(w) {
          message(paste("failure installing", pkg))
        })
      }
    })
  }

  if (requireNamespace("shiny", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE) &&
    requireNamespace("tibble", quietly = TRUE) &&
    requireNamespace("rlang", quietly = TRUE) &&
    requireNamespace("stringr", quietly = TRUE) &&
    requireNamespace("quantmod", quietly = TRUE) &&
    requireNamespace("rmarkdown", quietly = TRUE) &&
    requireNamespace("rhino", quietly = TRUE) &&
    requireNamespace("tinytex", quietly = TRUE) &&
    requireNamespace("pillar", quietly = TRUE) &&
    requireNamespace("rjson", quietly = TRUE) &&
    requireNamespace("lubridate", quietly = TRUE) &&
    requireNamespace("rmarkdown", quietly = TRUE) &&
    requireNamespace("readr", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE) &&
    requireNamespace("quantmod", quietly = TRUE) &&
    requireNamespace("shinyAce", quietly = TRUE) &&
    requireNamespace("fs", quietly = TRUE) &&
    requireNamespace("jsonlite", quietly = TRUE)

  ) {
    if (!rmarkdown::pandoc_available()) {
      warning(paste0("PATH var has:\n ", Sys.getenv("PATH"), "\nit is missing pandoc path, probably"))
      stop("Please check pandoc installation and add its path to the PATH variable", call. = FALSE)
    }
    ev <- tryCatch(shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE), error = function(e) {
      "error"
    })
    if (is.character(ev)) {
      if (ev == "error") {
        shiny::runApp(appDir, display.mode = "normal")
      }
    }
  } else {
    message(paste("Please install:", paste(missPkg, collapse = ", ")))
  }
}
