#' @import httr jsonlite purrr dplyr
onfleet_call <-
  function(type = c("GET", "POST", "PUT", "DELETE"),
           endpoint = c("auth/test", "organization", "organizations", "admins", "workers", "hubs", "teams", "destinations", "recipients", "tasks", "containers", "webhooks", "tasks/all"),
           body = NULL,
           query = NULL,
           id = NULL) {
    type <- match.arg(type)
    endpoint <- match.arg(endpoint)
    if (!is.null(id)) {
      endpoint <- paste0(endpoint, "/", id)
    }
    url <- switch(
      type,
      GET = modify_url(
        BASE_URL,
        path = paste0("api/v2/", endpoint),
        query = query
      ),
      modify_url(BASE_URL, path = paste0("api/v2/", endpoint))
    )
    
    resp <- switch(
      type,
      GET = GET(
        url,
        onfleet_auth(),
        httr::accept_json(),
        user_agent("CannaData")
      ),
      POST = POST(
        url,
        onfleet_auth(),
        body = body,
        encode = "json",
        httr::accept_json(),
        user_agent("CannaData"),
        httr::content_type_json()
      ),
      PUT = PUT(
        url,
        onfleet_auth(),
        body = body,
        encode = "json",
        httr::accept_json(),
        user_agent("CannaData"),
        httr::content_type_json()
      ),
      DELETE = DELETE(
        url,
        onfleet_auth(),
        httr::accept_json(),
        user_agent("CannaData")
      )
    )
    
    
    if (!(http_type(resp) %in% c("application/json", "text/json")) && http_error(resp)) {
      print(http_status(resp)$message)
      stop("onfleet API did not return JSON.", call. = FALSE)
    } else if (!(http_type(resp) %in% c("application/json", "text/json"))) {
      return(TRUE)
    }
    
    if (http_error(resp)) {
      print(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE))
      stop(paste("onfleet API errored:",
                 http_status(resp)$message, sep = "\\n"),
           call. = FALSE)
    }
    
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    
  }

onfleet_auth <- function(api_key = Sys.getenv("onfleet_api_key")) {
  authenticate(api_key, "")
}

#' @export
onfleet_auth_test <- function() {
  onfleet_call("GET", "auth/test")
}

BASE_URL <- "https://onfleet.com/"
