
# Formerly googleAuthR::gar_discovery_apis_list()
#' Get a list of Google API libraries
#' 
#' Does not require authentication
#' @param name Only include APIs with the given name. *Case-sensitive. All are lowercase currently.
#' @param preferred Return only the preferred version of an API.  FALSE by default.
#' 
#' @seealso \url{https://developers.google.com/discovery/v1/reference/apis/list}
#' 
#' @return List of Google APIs and their resources
#' @family API Discovery Service
#' 
#' @export
api_list <- function(name = NULL, preferred = NULL){
  
  name <- tolower(name)
  
  url <- "https://www.googleapis.com/discovery/v1/apis"
  query <- as.list(environment())
  req <- httr::RETRY("GET", url, query = query)
  
  httr::stop_for_status(req)
  
  stuff <- httr::content(req, as = "text")
  apis <- jsonlite::fromJSON(stuff)
  
  if(!is.null(apis$kind) && apis$kind == "discovery#directoryList"){
    out <- apis$items
  } else {
    stop("Problem fetching Discovery APIs")
  }
  
  out
}

# Formerly googleAuthR::gar_discovery_api()
#' Get meta data details for specified Google API
#' 
#' Download the discovery document for an API
#' 
#' @param api The API to fetch
#' @param version The API version to fetch
#' 
#' @seealso \url{https://developers.google.com/discovery/v1/getting_started}
#' 
#' @return Details of the API 
#' @family Google Discovery API functions
#' @export
api_get <- function(api, version){
  
  the_url <- glue::glue("https://www.googleapis.com/discovery/v1/apis/{api}/{version}/rest")
  
  req <- httr::RETRY("GET", the_url)
  
  httr::stop_for_status(req)
  
  stuff <- httr::content(req, as = "text")
  dd <- jsonlite::fromJSON(stuff)
  
  if(!is.null(dd$kind) && dd$kind == "discovery#restDescription"){
    out <- dd
  } else {
    stop("Problem fetching API Description")
  }
  
  out
}
