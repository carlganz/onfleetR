#' Manage Destinations
#' @seealso \url{http://docs.onfleet.com/docs/destinations} \url{http://docs.onfleet.com/docs/entities#destination}
#' @param name Name associated with address
#' @param number Number of address
#' @param street Street name
#' @param apartment Number of apartment (if applicable)
#' @param city City name
#' @param state State name
#' @param postalCode ZIP code
#' @param country Defaults to USA
#' @param unparsed Alternatively enter entire address string unparsed
#' @param notes Notes
#' @export
onfleet_post_destinations <- function(name = NULL, number, street, apartment = NULL,
                                      city, state = NULL, postalCode = NULL, country = "USA",
                                      unparsed = NULL, notes = NULL) {
  address <- list(name = name, number = number, street = street, apartment = apartment,
                  city = city, state = state, postalCode = postalCode, country = country,
                  unparsed = unparsed)
  
  onfleet_call("POST", "destinations", body = list(
    address = shiny:::dropNullsOrEmpty(address), notes = notes
  ))
}

#' @rdname onfleet_post_destinations
#' @param id ID of destination
#' @export
onfleet_get_destination <- function(id) {
  onfleet_call("GET", "destinations", id = id)
}
