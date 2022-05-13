
download_discovery_document2 <- function(id, path = NULL) {
  #   url <- make_discovery_url(id)
     url <- api_list() %>% dplyr::filter(id == !!id) %>% dplyr::pull(discoveryRestUrl)
     dd <- httr::GET(url)
     httr::stop_for_status(dd, glue::glue("find Discovery Document for ID '{id}'"))
     
     if (is.null(path)) {
          dd_content <- httr::content(dd)
          api_date <- dd_content[c("revision", "id")]
          api_date <- c(
               id = sub(":", "-", api_date$id),
               revision = as.character(as.Date(api_date$revision, format = "%Y%m%d"))
          )
          json_filename <- fs::path(paste(api_date, collapse = "_"), ext = "json")
          data_raw <- rprojroot::find_package_root_file("data-raw")
          path <- if (fs::dir_exists(data_raw)) {
               fs::path(data_raw, json_filename)
          } else {
               json_filename
          }
     }
     
     writeLines(httr::content(dd, as = "text"), path)
     path
}

# Based on suggestions from https://gargle.r-lib.org/articles/request-helper-functions.html
preprocess_methods <- function(api_id, dir = '.'){
     
     suppressMessages(
          source(
               system.file("discovery-doc-ingest", "ingest-functions.R", package = "gargle")
          )
     )
     
     filename <- api_id %>% str_replace_all('[:.]', '_') %>% paste0('.json')
     
     x <- download_discovery_document2(api_id, path = glue::glue('{dir}/{filename}'))
     
     dd <- read_discovery_document(x)
     
     methods <- get_raw_methods(dd)
     methods <- methods %>% map(groom_properties,  dd)
     
     suppressMessages(
          methods <- methods %>% map(add_schema_params, dd)
     )
     methods <- methods %>% map(add_global_params, dd)
     
     # Remove pesky parameters beginning with "$" character. $.xgafv for example
     methods <-
          methods %>% 
          map(
               function(method) {
                    method$parameters <- 
                         method$parameters %>% 
                         discard(names(.) %>% 
                                      str_detect(fixed('$'))) 
                    return(method)
               }
          )
     
     ## duplicate two methods to create a companion for media
     ## simpler to do this here, in data, than in wrapper functions
     
     #### Since googledrive package exists already probably safe to skip since add_global_params is also skipped.
     # mediafy <- function(target_id, methods) {
     #   new <- target_method <- methods[[target_id]]
     #   
     #   new$id <- paste0(target_id, ".media")
     #   new$path <-
     #     pluck(target_method, "mediaUpload", "protocols", "simple", "path")
     #   new$parameters <- c(
     #     new$parameters,
     #     uploadType = list(list(type = "string", required = TRUE, location = "query"))
     #   )
     #   
     #   methods[[new$id]] <- new
     #   methods
     # }
     # 
     #   if(str_detect(api_id, 'drive')){
     #        methods <- mediafy("drive.files.update", methods)
     #        methods <- mediafy("drive.files.create", methods)
     #  }
     
     # Add full scopes to methods as vectors.
     methods <- methods %>% purrr::map(~.x %>% purrr::list_modify(scopes = paste0('https://www.googleapis.com/auth/', .x$scopes %>% stringr::str_split(', ') %>% purrr::pluck(1))))
     
     # Google returns inconsistent ordering of methods.
     .endpoints <- methods[names(methods) %>% sort()] 
     
     attr(.endpoints, "base_url") <- dd$rootUrl
     
     ## View(.endpoints)
     
     list(dd, .endpoints)
}
