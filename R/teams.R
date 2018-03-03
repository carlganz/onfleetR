#' Manage Teams
#' @param name Unique name for team
#' @param workers Vector of worker IDs
#' @param managers Vector of admin IDs
#' @param hub ID of team hub
#' @seealso \url{http://docs.onfleet.com/docs/teams} \url{http://docs.onfleet.com/docs/entities#team}
#' @export
onfleet_post_teams <- function(name, workers = list(), managers = list(), hub = NULL) {
  onfleet_call("POST", "teams", body = list(
    name = name, workers = workers, managers = managers, hub = hub
  ))
}

#' @rdname onfleet_post_teams
#' @param id ID of team
#' @param ... values to update
#' @export
onfleet_put_teams <- function(id, ...) {
  onfleet_call("PUT", "teams", id = id, body = list(...))
}

#' @rdname onfleet_post_teams
#' @export
onfleet_get_teams <- function() {
  onfleet_call("GET", "teams") %>%
    map(shiny:::dropNullsOrEmpty) %>%
    bind_rows()
}

#' @rdname onfleet_post_teams
#' @export
onfleet_get_team <- function(id) {
  onfleet_call("GET", "teams", id = id)
}

#' @rdname onfleet_post_teams
#' @export
onfleet_delete_teams <- function(id) {
  onfleet_call("DELETE", "teams", id = id)
}