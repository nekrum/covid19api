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

#' Get DayOne cases
#'
#' Get all cases by type and country from the first recorded case.
#' Country must be the slug from GetAvalaibleCountries() or GetCountrySummary(). Cases
#' must be one of: confirmed, recovered, deaths. When total parameter is TRUE the live
#' parametersis not necesary.
#'
#' @param country.requested Country slug name choosed
#' @param status.requested Status requested, they could be confirmed, recovered or deaths
#' @param live If TRUE gets the lates cases from the country and status requested
#' @param total If TRUE returns all cases by type for a country from the first recorded case
#'
#' @return Data frame columns country, Province, latitude, longitude, date, number of cases and status
#' @export
#'
#' @examples GetDayOne(country.requested = 'mexico', status.requested = 'confirmed')
#' @examples GetDayOne(country.requested = 'mexico', status.requested = 'confirmed', live = TRUE)
#' @examples GetDayOne(country.requested = 'mexico', status.requested = 'confirmed', total = TRUE)
GetDayOne <- function(country.requested, status.requested, live = FALSE, total = FALSE){
  total <- ifelse(total, 'total', "")
  live <- ifelse(live & total == FALSE, '/live', "")
  dayone.request <- glue::glue(
    "{total}/dayone/country/{country.requested}/status/{status.requested}{live}", .trim = T
  )
  dayone.resp <- CovidAPI(dayone.request)
  dayone.data <- jsonlite::fromJSON(
    httr::content(dayone.resp, "text", encoding = "UTF-8"),
    simplifyDataFrame = T
  )
  return(dayone.data)
}

#' Get cases by Country
#'
#' Get all by type cases for a country. Country must be the slug from
#' GetAvalaibleCountries() or GetCountrySummary(). Cases must be one of:
#' confirmed, recovered, deaths. When total parameter is TRUE the live
#' parameter is not necesary.
#'
#' @param country.requested Country slug name choosed
#' @param status.requested Status requested, they could be confirmed, recovered or deaths
#' @param live If TRUE gets the lates cases from the country and status requested
#' @param total If TRUE returns all cases by type for a country from the first recorded case
#'
#' @return Data frame columns country, Province, latitude, longitude, date, number of cases and status
#' @export
#'
#' @examples GetByCountry(country.requested = 'mexico', status.requested = 'confirmed')
#' @examples GetByCountry(country.requested = 'mexico', status.requested = 'confirmed', live = TRUE)
#' @examples GetByCountry(country.requested = 'mexico', status.requested = 'confirmed', total = TRUE)
GetByCountry <- function(country.requested, status.requested, live = FALSE, total = FALSE){
  total <- ifelse(total, 'total', "")
  live <- ifelse(live & total == FALSE, '/live', "")
  bycountry.request <- glue::glue(
    "{total}/country/{country.requested}/status/{status.requested}{live}", .trim = T
  )
  bycountry.resp <- CovidAPI(bycountry.request)
  bycountry.data <- jsonlite::fromJSON(
    httr::content(bycountry.resp, "text", encoding = "UTF-8"),
    simplifyDataFrame = T
  )
  return(bycountry.data)
}

#' Get Live cases for country
#'
#' Get all live cases by  type for a country. Country must be the slug from
#' GetAvalaibleCountries() or GetCountrySummary(). Cases must be one of:
#' confirmed, recovered, deaths.
#' It could be filter by date an time.
#'
#' @param country.requested Country slug name choosed
#' @param status.requested Status requested, they could be confirmed, recovered or deaths
#' @param after.date Initial date to download data
#' @param after.time Initial time to donload data, '00:00:00' is default
#'
#' @return Data frame columns country, Province, latitude, longitude, datetime, number of cases and status
#' @export
#'
#' @examples GetLiveCountry(country.requested = 'mexico', status.requested = 'confirmed')
#' @examples GetLiveCountry(
#'   country.requested = 'mexico',
#'   status.requested = 'confirmed',
#'   after.date = '2020-03-27'
#' )
#' @examples GetLiveCountry(
#'   country.requested = 'mexico',
#'   status.requested = 'confirmed',
#'   after.date = '2020-03-27',
#'   after.time = '17:57:00'
#' )
GetLiveCountry <- function(country.requested, status.requested, after.date = NA, after.time = '00:00:00'){
  if(!is.na(after.date)) {
    after.date <- glue::glue("/date/{after.date}T{after.time}Z")
  }
  live.request <- glue::glue(
    "live/country/{country.requested}/status/{status.requested}{after.date}", .trim = T, .na = ""
  )
  live.resp <- CovidAPI(live.request)
  live.data <- jsonlite::fromJSON(
    httr::content(live.resp, "text", encoding = "UTF-8"),
    simplifyDataFrame = T
  )
  return(live.data)
}

