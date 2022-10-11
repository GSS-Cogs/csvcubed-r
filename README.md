
<!-- README.md is generated from README.Rmd. Please edit that file -->

# csvcubedr

<!-- badges: start -->
<!-- badges: end -->

This R package benefits R users by describing their CSV files to improve
the R user experience of Csvcubed. The package aims to provide valuable,
intuitive and adequately powerful functions to configure the data frame
as the user wishes to and generates the JSON configuration file. Besides
the basic functionality, the project includes other user-friendly
features to improve the user experience. For example, the project
includes built-in auto-fill lists to help users to fill in the
information efficiently and accurately. Also, it produces error messages
to guide the user to correct their input and eventually make a
functioning configuration.

## Installation

You can install the development version of csvcubedr like so:

``` r
install.packages("devtools")
devtools::install_github("GSS-Cogs/csvcubed-r")

library(csvcubedr)
```

## Example

Creating a configuration for the example data included library:

``` r
library(csvcubedr)
library(magrittr)
config <- create.config(example.data,
  title = "Sweden at Eurovision",
  summary = "List of Swedish entries to the Eurovision Song Contest since 1958.",
  license = "https://creativecommons.org/licenses/by/4.0/",
  publisher = Organizations$Open_Knowledge_Foundation,
  dataset.issued = "2022-04-08",
  keywords = c("Eurovision", "Song Contest", "Sweden", "European Broadcasting Union")
)
```

Adding a dimension column:

``` r
config <- add.dimension.column(config, column.name = "Entrant", label = "Entrant")
```

Adding a dimension from a template:

``` r
config <- add.year.column(config, column.name = "Year")
```

Adding a Attribute column. see help(values), and help(AttributeValues):

``` r
config <- add.attribute.column(config, column.name = "Status", values = values(attribute.value(label = "Final"), attribute.value(label = "Provisional")))
```

Adding a Measure column:

``` r
config <- add.measure.column(config,
  column.name = "Measure",
  values = values(
    measure.value(label = "Final Rank"),
    measure.value(label = "Final Points"), measure.value(label = "People on Stage")
  )
)
```

Adding a observation column:

``` r
config <- add.observation.column(config, column.name = "Value")
```

Adding a unit column:

``` r
config <- add.unit.column(config,
  column.name = "Unit",
  unit.values = unit.values("Numberless", "Unitless")
)
```

Finally, generate the JSON metadata called “my-data.json”, use the
commented code(we have prevented generating a file):

``` r
#generate.json.configuration(config, "my-data.json")
```

Make sure your csv file has the same name, use the commented code(we
have prevented generating a file):

``` r
#write.csv(example.data, "my-data.csv")
```
