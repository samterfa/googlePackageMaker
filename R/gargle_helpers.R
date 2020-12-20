# NEW FUNCTION PARADIGM
# .endpoints <- preprocess_methods('admin:directory_v1') (on package load) googledrive:::.endpoints is a good example
# load endpoint using equivalent of googledrive::drive_endpoint().
# get token somehow
# req <- request_develop(endpoint = admin_directory_v1_endpoint('admin.users.patch'), params = list(userKey = emailAddress, notes = 'testing'))
# req <- request_build(method = req$method, path = req$path, params = req$params, body = req$body, token = httr::config(token = token))
# temp <- request_make(req, encode = 'json')

# Based on suggestions from https://gargle.r-lib.org/articles/request-helper-functions.html
preprocess_methods <- function(api_id, dir = '.'){
     
     require(tidyverse)
     
     source(
          system.file("discovery-doc-ingest", "ingest-functions.R", package = "gargle")
     )
     
    filename <- api_id %>% str_replace_all('[:.]', '_') %>% paste0('.json')
     x <- download_discovery_document(api_id, path = glue::glue('{dir}/{filename}'))
     dd <- read_discovery_document(x)
     
     methods <- get_raw_methods(dd)
     
     methods <- methods %>% map(groom_properties,  dd)
     methods <- methods %>% map(add_schema_params, dd)
  ###   methods <- methods %>% map(add_global_params, dd)
     
     ## duplicate two methods to create a companion for media
     ## simpler to do this here, in data, than in wrapper functions
     mediafy <- function(target_id, methods) {
          new <- target_method <- methods[[target_id]]
          
          new$id <- paste0(target_id, ".media")
          new$path <-
               pluck(target_method, "mediaUpload", "protocols", "simple", "path")
          new$parameters <- c(
               new$parameters,
               uploadType = list(list(type = "string", required = TRUE, location = "query"))
          )
          
          methods[[new$id]] <- new
          methods
     }
    
     # Since googledrive package exists already probably safe to skip since add_global_params is also skipped.
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