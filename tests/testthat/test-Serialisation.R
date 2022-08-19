library(tibble)
library(magrittr)
library(csvcubedconfiggenerator)

# This test currently fails with:
#
# Failure (test-Serialisation.R:29:3): Month columns are correctly serialised to JSON
# jsonlite::fromJSON(config_json) (`actual`) not equal to list(...) (`expected`).
#
# `actual$public_contact_point_uri`:   "Warning: 'public.contact.point.uri' must be an URL ."
# `expected$public_contact_point_uri`: "mailto:some.one@example.com" 
#
# This highlights that the `public_contact_point_uri` isn't correctly being pulled through.

test_that("Month columns are correctly serialised to JSON", {
  df <- tibble(
    `Month` = c("2020-02", "2020-03", "2020-04"),
    `Column B` = c(4, 5, 6)
  )

  config_json <- config_csv(
    df,
    id = "Some identifier",
    title = "Some title",
    description = "Some description",
    summary = "Some summary",
    publisher = Publishers$`Open Knowledge Foundation`,
    creator = Publishers$`Open Knowledge Foundation`,
    themes = c("http://some-theme", "http://some-other-theme"),
    keywords = c("Keyword 1", "Keyword two"),
    dataset.issued = "2020-01-02",
    dataset.modified = "2010-02-23",
    license = "http://some-license",
    public.contact.point.uri = "mailto:some.one@example.com"
  ) %>%
    add.month.column("Month") %>%
    generate.json.configuration()

  expect_equal(
    jsonlite::fromJSON(config_json),
    list(
      `$schema` = "https://purl.org/csv-cubed/qube-config/v1",
      id = "Some identifier",
      title = "Some title",
      summary = "Some summary",
      publisher = "http://dbpedia.org/resource/Open_Knowledge_Foundation",
      creator = "http://dbpedia.org/resource/Open_Knowledge_Foundation",
      themes = c("http://some-theme", "http://some-other-theme"),
      keywords = c("Keyword 1", "Keyword two"),
      dataset_issued = "2020-01-02T00:00:00",
      dataset_modified = "2010-02-23T00:00:00",
      license = "http://some-license",
      public_contact_point_uri = "mailto:some.one@example.com",
      columns = list(
        Month = list(
          type = "dimension",
          from_template = "month"
        )
      )
    )
  )
})
