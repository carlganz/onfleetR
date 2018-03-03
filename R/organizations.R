#' Get info about organizations
#' @seealso \url{http://docs.onfleet.com/docs/organizations} \url{http://docs.onfleet.com/docs/entities#organization}
#' @export
onfleet_get_organizations <- function() {
  onfleet_call("GET", "organization")
}

#' @rdname onfleet_get_organization
#' @param id ID of organization
#' @export
onfleet_get_organization <- function(id) {
  onfleet_call("GET", "organizations", id = id)
}