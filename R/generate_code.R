this_package <- 'googlePackageMaker'

# Formerly googleAuthR::gar_create_api_skeleton()
#' Create an API library skeleton
#' 
#' This will create a file with the skeleton of the API functions 
#'   for the specified library
#' 
#' @api_name The api to fetch. Run \code{\link[{this_package}]{list_google_apis} for a list of options.
#' @api_version The api version to fetch. Run \code{\link[{this_package}]{list_google_apis} for a list of options.
#' @param output_dir Directory path to write the package to.
#' @param package_name Name of the package to create.
#' 
#' @return TRUE if successful, side effect will write a file.
#' @family Google Discovery API functions
#' @export
make_google_package <- function(api_name,
                                api_version,
                                output_dir,
                                package_name){
     
   # Load API info
   api_info <- get_google_api(api_name, api_version)
   
   ############ Create package file structure ############
   final_package_path <- glue::glue('{output_dir}/{package_name}') %>% sub(pattern = '//', replacement = '/')
   
   if(dir.exists(final_package_path)){
      readline(glue::glue('{final_package_path} exists and will be overwritten! Press ESC to abort.'))
   }
   
   if(!dir.exists(output_dir)) dir.create(output_dir)
   if(!dir.exists(tempdir())) dir.create(tempdir())
   
   temp_package_dir <- glue::glue('{tempdir()}/{package_name}')
   if(!dir.exists(temp_package_dir)) dir.create(temp_package_dir)
   
   temp_script_dir <- glue::glue('{temp_package_dir}/R')
   if(!dir.exists(temp_script_dir)) dir.create(temp_script_dir)
   
   # Create a file per API resource.
   for(resource in names(api_info$resources)) file.create(glue::glue('{temp_script_dir}/{resource}.R'))
   
   # Create scopes file for scopes functions.
   file.create(glue::glue('{temp_script_dir}/scopes.R'))
   
   # Create schemas file for schema functions.
   file.create(glue::glue('{temp_script_dir}/schemas.R'))
   
   
   ############  Create get_function_scopes function. ############
   
   all_scopes <- api_info$auth$oauth2$scopes %>% names()
   
   scopes_text <- glue::glue("\n\t",
                             "#' Get a list of scopes needed for an API function",
                             "\n\t",
                             "#' This function returns the list of needed scopes for a given API function.",
                             "\n\t",
                             "#'",
                             "' @seealso \\href{{https://developers.google.com/identity/protocols/oauth2/scopes}}",
                             "\n\t",
                             "#' @param function_name The name of the API function to return scopes for.",
                             "\n\t",
                             "#' @param return_all Whether to return all scopes for the API. Defaults to FALSE.",
                             "\n\t",
                             "#' @export",
                             "\n\t",
                             "get_function_scopes <- function(function_name = NULL, return_all = F){{",
                             "\n\n\t\t",
                             "# Return all scopes",
                             "\n\t\t",
                             "scopes <- c(",
                             .trim = F)
   
   for(scope in all_scopes){
      scopes_text <- glue::glue("{scopes_text}",
                                "\n\t\t\t'{scope}'",
                                ifelse(last(all_scopes) != scope, ",", ")"),
                                .trim = F)
   }
   
   scopes_text <- glue::glue("{scopes_text}",
                             "\n\n\t\t",
                             "if(return_all) return(scopes)",
                             .trim = F)
   
   ############  Create schema functions. ############
   for(schema_info in api_json$schemas){
      
      schema_doc_text <- glue::glue("\n\t",
                                    "#' Create {schema_info$id} object",
                                    "\n\t",
                                    "#' {schema_info$description}",
                                    "\n\t",
                                    .trim = F)
                                    
      for(param in names(schema_info$properties)){
         
         param_info <- schema_info$properties[param]
         
         schema_doc_text <- glue::glue("\n\t",
                                       "#' @param {param} ",
                                       "\n\t",
                                       "#' {schema_info$description}",
                                       "\n\t",
                                       .trim = F)
         
      }
      
      
   }
   
   
     # Form strings which can be evaluated to subset json documentation for methods
     methods <- api_info$resources %>% unlist(recursive = T) %>% names() %>%purrr::keep(~(.x %>% stringr::str_detect('methods.*.id')) & (.x %>% stringr::str_detect('.id$'))) %>% stringr::str_replace_all('.id$', '')
     method_docs <- paste0('api_info$resources$', methods %>% stringr::str_replace_all(stringr::fixed('.'), '$')) %>% sort()
     base_url <- api_info$rootUrl   ### NOT baseUrl since gargle doesn't handle multiple forward slashes in the base_url field of a request.
     path_prefix <- sub(api_info$rootUrl, '', api_info$baseUrl)
     
     # https://developers.google.com/discovery/v1/reference/apis
     for(method_doc in method_docs){
          
          # category of the function and file destination of the function code
          category <- method_doc %>% stringr::str_replace(stringr::fixed('api_info$resources$'), '') %>% stringr::str_sub(1,stringr::str_locate(., stringr::fixed('$'))[[1]] - 1)
          
          # documentation json subsetted by method
          method_info <- eval(parse(text = method_doc))
          
          # function name i function id. may revisit
          function_id <- method_info$id
          function_name <- substr(x = function_id, start = regexpr(text = function_id, pattern = '.', fixed = T)[[1]] + 1, stop = nchar(function_id))
          
          # description of function
          function_description <- method_info$description
          
          # path for api calls. Not sure whether to use flatPath or path yet.
          path <- paste0(path_prefix, method_info$path)
          
          # api method for call
          method <- method_info$httpMethod
          
          # vector of scopes needed to make the given api call
          scopes <- method_info$scopes
          
          # relevant schema for body of the request
          if(!is.null(method_info$request)){
               
               body_schema_ref <- method_info$request$`ref`
               
               body_schema <- api_info$schemas[body_schema_ref]
          }else{
               
               body_schema_ref <- NULL
               
               body_schema <- NULL
          }
          
          # relevant schema for response of the request
          if(!is.null(method_info$request)){
               
               response_schema_ref <- method_info$request$`ref`
               
               response_schema <- api_info$schemas[response_schema_ref]
          }else{
                  
                  response_schema_ref <- NULL
                  
                  if(length(method_info$response$`$ref`) > 0){
                          response_schema <- api_info$schemas[[method_info$response$`$ref`]]
                  }else{
                          response_schema <- NULL
                  }
          }
          
          # parameter names ordered - likely not important because gargle takes care of substitutions
          param_order <- method_info$parameterOrder
          
          # list of path and query parameters for api call ordered by param_order
          params <- method_info$parameters
          if(length(param_order) > 0) params <- params[param_order]
          
          
############ Generate documentation text ############
          doc_text <- glue::glue(
               
               "\n\t",
               "#' {function_description}",
               "\n\t",
               "#'",
               "\n\t",
               "#' Autogenerated via \\code{{\\link[{this_package}]{{generate_google_package}}}}",
               "\n\t",
               "#'",
               "\n\t",
               "#' @seealso \\href{{{api_info$documentationLink}}}{{Google Documentation}}",
               "\n\t",
               "#'",
               "\n\t",
               "#' @details",
               "\n\t",
               "#' Authentication scopes used by this function are:",
               "\n\t",
               "#' \\itemize{{", 
               
               .trim = F)
          
          for(scope in scopes){
               
               doc_text <- glue::glue("{doc_text}",
                                     "\n\t",
                                     "#' \\item {scope}", 
                                     .trim = F)
          }
          
          doc_text <- glue::glue("{doc_text}", 
                                "\n\t",
                                "#' }}", 
                                .trim = F)
          
          for(param in names(params)){
               
               param_info <- method_info$parameters[[param]]
               
               doc_text <- glue::glue("{doc_text}",
                                     "\n\t",
                                     "#' @param {param} {param_info$description}", 
                                     .trim = F)
          }
          
          # Add system param "fields".
          doc_text <- glue::glue("{doc_text}",
                                 "\n\t",
                                 "#' @param fields {api_info$parameters$fields$description}",
                                 ifelse(!is.null(response_schema), " Possible fields are {paste({response_schema$properties %>% names()}, collapse = ', ')}.", ''),
                                 .trim = F)
          
          # Add token and return_response as well.
          doc_text <- glue::glue("{doc_text}", 
                                "\n\t",
                                "#' @param token A token prepared by one of gargle's token generating functions. Defaults to gargle::token_fetch(scopes = \\code{{\\link[{this_package}]{{get_function_scopes}}}}(function_name). See \\code{{\\link[gargle]{{token_fetch}}}} for more info.",
                                "\n\t",
                                "#' @param return_response Whether to return the response or the response content. Defaults to FALSE (return response content).", 
                                .trim = F)
          
          doc_text <- glue::glue("{doc_text}",
                                "\n\t",
                                "#' @export", 
                                .trim = F)
          
############ Generate function text ###############
          
          function_text <- glue::glue("\t",
                                     "{function_name} <- function(",
                                     .trim = F)
          
          for(param in names(params)){
                function_text <- glue::glue("{function_text}", "{param} = NULL, ")
          }
          
          # Finish off function parameters list
          function_text <- glue::glue("{function_text}",
                                      "token = gargle::token_fetch(scopes = get_function_scopes('{function_name}')), return_response = F, fields = NULL){{",
                                      .trim = F)
          
          # Add scopes for the new function to get_function_scopes.
          scopes_text <- glue::glue("{scopes_text}",
                                    "\n\n\n\t\t",
                                    "# Return scopes for {function_name}",
                                    "\n\t\t",
                                    "scopes <- c(",
                                    .trim = F)
          
          for(scope in scopes){
                  scopes_text <- glue::glue("{scopes_text}",
                                            "\n\t\t\t'{scope}'",
                                            ifelse(last(scopes) != scope, ",", ")"),
                                            .trim = F)
          }
          
          scopes_text <- glue::glue("{scopes_text}",
                                    "\n\n\t\t",
                                    "if(function_name == '{function_name}') return(scopes)",
                                    .trim = F)
          
          # Build function
          # TODO: NEED TO ADD useragent to request_make.
          function_text <- glue::glue("{function_text}", 
                                     "\n\t\t",
                                     "params <- as.list(environment())[!names(as.list(environment())) %in% c('body', 'return_response', 'token')]",
                                     "\n\t\t",
                                     "req <- gargle::request_build(method = '{method}', path = '{path}', params = params, body = NULL, token = token, base_url = '{base_url}')",
                                     "\n\t\t",
                                     "res <- gargle::request_make(req, encode = 'json')",
                                     "\n\t\t",
                                     "if(return_response) return(res)",
                                     "\n\t\t",
                                     "httr::content(res)",
                                     "\n\t",
                                     "}",
                                     .trim = F)
          
          # Update appropriate file with new documentation and function.
          readr::write_lines(doc_text, glue::glue('{temp_script_dir}/{category}.R'), append = T)
          readr::write_lines(function_text, glue::glue('{temp_script_dir}/{category}.R'), append = T)
     }
     
     scopes_text <- glue::glue("{scopes_text}",
                               "\n\t",
                               "}",
                               .trim = F)
     
     # Write scopes function to file.
     readr::write_lines(scopes_text, glue::glue('{temp_script_dir}/scopes.R'), append = F)
     
     # Output package files
     file.copy(from = temp_package_dir, to = output_dir, recursive = T, overwrite = T)
     
     T
}
