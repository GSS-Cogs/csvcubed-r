# On OS X, Make sure you have run: `brew install libgit2`
# as well as having all of the xcode tools installed.


install_development_tools <- function() {
    repos <- "https://cran.ma.imperial.ac.uk/"

    install.packages("devtools", repos = repos)
    install.packages("styler", repos = repos)
    install.packages("testthat", repos = repos)

    # Install vscDebugger - Useful for debugging in vscode.
    devtools::install_github("https://github.com/ManuelHentschel/vscDebugger")
}

install_local_pkg <- function() {
    devtools::install(".", reload = TRUE)
}

reformat_code <- function() {
    styler::style_pkg()
}

run_tests <- function() {
    devtools::test()
}