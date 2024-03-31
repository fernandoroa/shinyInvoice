<img src=man/figures/logo.png align="right" width="12%" hspace="50">

# shinyInvoice <br><br><br>

### Generate a Pdf Invoice with 'Rmarkdown'<br>

In this repository the app in the `inst` folder is a submodule  
Module Repository: https://github.com/fernandoroa/invoice-public

#### Install package

```
install.packages("shinyInvoice")
```

#### Install from github

```
devtools::install_github("fernandoroa/shinyInvoice",
                         ref = "main"
                        )
```

#### How to run app in temp folder (default)

After installing the package, run the function `shinyInvoice::runInvoice()`

#### How to run in a local folder

```

# go to your desired working folder in R
(setwd...)

# Having the package installed,
# copy the app to the working folder:
file.copy(system.file("shinyApps", "invoice_app", package = "shinyInvoice"), getwd(), recursive = TRUE)

# Read the main file
main <- readLines("invoice_app/app/main.R")

# Change string to use the local mode
main <- sub("local_safe_computer_mode <- FALSE", "local_safe_computer_mode <- TRUE", main)
writeLines(main, "invoice_app/app/main.R")
shiny::runApp("invoice_app/app.R")
```

This way when you save changes or do any download action your files will be (also) in the local folder:

```
invoice_app/app/tmp_dir/
```
