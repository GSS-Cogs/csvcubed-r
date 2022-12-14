% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/observation_code.R
\name{add.observation.column}
\alias{add.observation.column}
\title{add a observation column}
\usage{
add.observation.column(
  config,
  column.name,
  data.type = "",
  measure = "",
  unit = ""
)
}
\arguments{
\item{config}{A configuration object.The configuration object to add the column to.}

\item{column.name}{String. A column name in the Data Frame.}

\item{data.type}{String. "The data type of the attribute values". Can use the built-in list "DataTypes" to auto-fill.}

\item{measure}{A value object. The characteristic which was measured in all observation. Use measure.value() to create such object for this argument.}

\item{unit}{A unit object. The unit that all observations were measured in. Use unit.value() to create such object for this argument.}
}
\description{
add a observation column to the configuration object.
}
\examples{
data(example.data)
config <- create.config(example.data, title = "sweden_at_eurovision_no_missing")
config <- add.observation.column(config, column.name = "Value")
}
