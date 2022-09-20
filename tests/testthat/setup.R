venv_name <- "csvcubed-r-json-validation-2"
reticulate::virtualenv_create(venv_name)
reticulate::virtualenv_install(venv_name, "jsonschema")
reticulate::virtualenv_install(venv_name, "requests")
reticulate::use_virtualenv(venv_name)
reticulate::source_python("/Users/haoquanliu/Desktop/卡迪夫学习/Dissertation/validator.py")
data("example.data")
