#' Calls Covid19 API
#'
#' @param path API path response
#'
#' @return parsed response
#' @export
#'
#' @examples CovidAPI("summary")
CovidAPI <- function(path) {
  url <- httr::modify_url("https://api.covid19api.com/", path = path)
  resp <- httr::GET(url)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  jsonlite::fromJSON(httr::content(resp, "text"), simplifyDataFrame = T)
}


