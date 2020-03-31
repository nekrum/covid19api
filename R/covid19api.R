#' Calls Covid19 API
#'
#' @param path API path response
#'
#' @return Get json requested by path string
#' @export
#'
#' @examples CovidAPI("summary")
CovidAPI <- function(path) {
  url <- httr::modify_url("https://api.covid19api.com/", path = path)
  covid.resp <- httr::GET(url)
  if (httr::http_type(covid.resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  return(covid.resp)
}

#' Get country summary
#'
#' Obtain data summary of new and total cases per country updated daily and
#' parse extraction datetime and country data.
#'
#' @return Data Frame with the
#' @export
#'
#' @examples GetCountrySummary()
GetCountrySummary <- function() {
  summary.resp <- CovidAPI("summary")
  summary.data <- jsonlite::fromJSON(
    httr::content(summary.resp, "text", encoding = "UTF-8"),
    simplifyDataFrame = T
  )
  extraction.datetime <- lubridate::as_datetime(summary.data$Date)
  summary.data <- summary.data$Countries
}

#' Get Countries Available in database
#'
#' This functions get all the countries in database, with the slug
#' name and provinces if avalaible.
#'
#' @return data.frame structure with country name, sluj name and provinces
#' @export
#'
#' @examples GetAvalaibleCountries()
GetAvalaibleCountries <- function() {
  countries.resp <- CovidAPI("countries")
  countries.data <- jsonlite::fromJSON(
    httr::content(countries.resp, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  countries.data <- data.table::rbindlist(countries.data)
  for (j in seq_len(ncol(countries.data)))
    data.table::set(countries.data, which(countries.data[[j]] == ""), j, NA)
  return(countries.data[])
}
