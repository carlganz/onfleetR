#' Manage administrators
#' @param name Name of new administrator
#' @param email Email of new administrator
#' @param phone Phone number of new administrator
#' @param isReadOnly Indicate whether admin is read-only
#' @seealso \url{http://docs.onfleet.com/docs/administrators} \url{http://docs.onfleet.com/docs/entities#administrator}
#' @export
onfleet_post_admins <- function(name, email, phone = NULL, isReadOnly = NULL) {
  onfleet_call("POST", "admins", body = list(name = name, email = email, phone = phone, isReadOnly = isReadOnly))
}

#' @rdname onfleet_post_admins
#' @export
onfleet_get_admins <- function() {
  onfleet_call("GET", "admins") %>%
    map(shiny:::dropNullsOrEmpty) %>%
    bind_rows()
}

#' @param id ID of administrator
#' @param ... values to update
#' @rdname onfleet_post_admins
#' @export
onfleet_put_admins <- function(id, ...) {
  onfleet_call("PUT", "admins", id = id, body = list(...))
}

#' @rdname onfleet_post_admins
#' @export
onfleet_delete_admins <- function(id) {
  onfleet_call("DELETE", "admins", id = id)
}