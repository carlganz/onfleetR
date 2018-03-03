#' Get hubs
#' @seealso \url{http://docs.onfleet.com/docs/hubs} \url{http://docs.onfleet.com/docs/entities#hub}
#' @export
onfleet_get_hubs <- function() {
  onfleet_call("GET", "hubs")
}
