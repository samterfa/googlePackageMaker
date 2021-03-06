
	#' Deletes a domain of the customer.
	#'
	#'  Autogenerated via \code{\link[googlePackageMaker]{package_make}}.
	#'
	#' @seealso \href{http://developers.google.com/admin-sdk/}{Google Documentation}
	#'
	#' @details
	#' Authentication scopes used by this function are:
	#' \itemize{
	#' \item https://www.googleapis.com/auth/admin.directory.domain
	#' }
	#' @param customer Immutable ID of the Google Workspace account.
	#' @param domainName Name of domain to be deleted
	#' @param gargle_token A token prepared by one of gargle's token generating functions. Defaults to gargle::token_fetch(...) with appropriate scopes. See \code{\link[gargle]{token_fetch}} for more info.
	#' @param return_response Whether to return the response or the response content. Defaults to FALSE (return response content).
	#' @export
	domains.delete <- function(customer, domainName, gargle_token = gargle::token_fetch(scopes = .endpoints[['directory.domains.delete']]$scopes), return_response = F){
		params <- as.list(environment())[!names(as.list(environment())) %in% c('return_response', 'gargle_token')]
		req <- gargle::request_develop(endpoint = .endpoints[['directory.domains.delete']], params = params, base_url = 'https://admin.googleapis.com/')
		req <- gargle::request_build(method = req$method, path = req$path, params = req$params, body = req$body, token = httr::config(token = gargle_token), base_url = req$base_url)
		res <- gargle::request_make(req, encode = 'json')
		if(return_response) return(res)
		httr::content(res)
	}

	#' Retrieves a domain of the customer.
	#'
	#'  Autogenerated via \code{\link[googlePackageMaker]{package_make}}.
	#'
	#' @seealso \href{http://developers.google.com/admin-sdk/}{Google Documentation}
	#'
	#' @details
	#' Authentication scopes used by this function are:
	#' \itemize{
	#' \item https://www.googleapis.com/auth/admin.directory.domain
	#' \item https://www.googleapis.com/auth/admin.directory.domain.readonly
	#' }
	#' @param customer Immutable ID of the Google Workspace account.
	#' @param domainName Name of domain to be retrieved
	#' @param gargle_token A token prepared by one of gargle's token generating functions. Defaults to gargle::token_fetch(...) with appropriate scopes. See \code{\link[gargle]{token_fetch}} for more info.
	#' @param return_response Whether to return the response or the response content. Defaults to FALSE (return response content).
	#' @export
	domains.get <- function(customer, domainName, gargle_token = gargle::token_fetch(scopes = .endpoints[['directory.domains.get']]$scopes), return_response = F){
		params <- as.list(environment())[!names(as.list(environment())) %in% c('return_response', 'gargle_token')]
		req <- gargle::request_develop(endpoint = .endpoints[['directory.domains.get']], params = params, base_url = 'https://admin.googleapis.com/')
		req <- gargle::request_build(method = req$method, path = req$path, params = req$params, body = req$body, token = httr::config(token = gargle_token), base_url = req$base_url)
		res <- gargle::request_make(req, encode = 'json')
		if(return_response) return(res)
		httr::content(res)
	}

	#' Inserts a domain of the customer.
	#'
	#'  Autogenerated via \code{\link[googlePackageMaker]{package_make}}.
	#'
	#' @seealso \href{http://developers.google.com/admin-sdk/}{Google Documentation}
	#'
	#' @details
	#' Authentication scopes used by this function are:
	#' \itemize{
	#' \item https://www.googleapis.com/auth/admin.directory.domain
	#' }
	#' @param customer Immutable ID of the Google Workspace account.
	#' @param kind Kind of resource this is.
	#' @param domainAliases List of domain alias objects. (Read-only)
	#' @param domainName The domain name of the customer.
	#' @param creationTime Creation time of the domain. Expressed in [Unix time](http://en.wikipedia.org/wiki/Epoch_time) format. (Read-only).
	#' @param etag ETag of the resource.
	#' @param isPrimary Indicates if the domain is a primary domain (Read-only).
	#' @param verified Indicates the verification state of a domain. (Read-only).
	#' @param gargle_token A token prepared by one of gargle's token generating functions. Defaults to gargle::token_fetch(...) with appropriate scopes. See \code{\link[gargle]{token_fetch}} for more info.
	#' @param return_response Whether to return the response or the response content. Defaults to FALSE (return response content).
	#' @export
	domains.insert <- function(customer, kind = NULL, domainAliases = NULL, domainName = NULL, creationTime = NULL, etag = NULL, isPrimary = NULL, verified = NULL, gargle_token = gargle::token_fetch(scopes = .endpoints[['directory.domains.insert']]$scopes), return_response = F){
		params <- as.list(environment())[!names(as.list(environment())) %in% c('return_response', 'gargle_token')]
		req <- gargle::request_develop(endpoint = .endpoints[['directory.domains.insert']], params = params, base_url = 'https://admin.googleapis.com/')
		req <- gargle::request_build(method = req$method, path = req$path, params = req$params, body = req$body, token = httr::config(token = gargle_token), base_url = req$base_url)
		res <- gargle::request_make(req, encode = 'json')
		if(return_response) return(res)
		httr::content(res)
	}

	#' Lists the domains of the customer.
	#'
	#'  Autogenerated via \code{\link[googlePackageMaker]{package_make}}.
	#'
	#' @seealso \href{http://developers.google.com/admin-sdk/}{Google Documentation}
	#'
	#' @details
	#' Authentication scopes used by this function are:
	#' \itemize{
	#' \item https://www.googleapis.com/auth/admin.directory.domain
	#' \item https://www.googleapis.com/auth/admin.directory.domain.readonly
	#' }
	#' @param customer Immutable ID of the Google Workspace account.
	#' @param gargle_token A token prepared by one of gargle's token generating functions. Defaults to gargle::token_fetch(...) with appropriate scopes. See \code{\link[gargle]{token_fetch}} for more info.
	#' @param return_response Whether to return the response or the response content. Defaults to FALSE (return response content).
	#' @export
	domains.list <- function(customer, gargle_token = gargle::token_fetch(scopes = .endpoints[['directory.domains.list']]$scopes), return_response = F){
		params <- as.list(environment())[!names(as.list(environment())) %in% c('return_response', 'gargle_token')]
		req <- gargle::request_develop(endpoint = .endpoints[['directory.domains.list']], params = params, base_url = 'https://admin.googleapis.com/')
		req <- gargle::request_build(method = req$method, path = req$path, params = req$params, body = req$body, token = httr::config(token = gargle_token), base_url = req$base_url)
		res <- gargle::request_make(req, encode = 'json')
		if(return_response) return(res)
		httr::content(res)
	}
