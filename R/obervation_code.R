add.observation.configuration <- function(config,
                                          column.name = "",
                                          measure = "",
                                          unit = "") {
  config <- add.column.configuration(config,
    column.name = column.name,
    measure = measure,
    unit = unit,
    type = "observations"
  )
  return(config)
}
