#' Manage Workers
#' @param name Name of new worker
#' @param phone Phone number of new worker
#' @param teams Vector of IDs indicating which teams new worker is member of
#' @param vehicleType Either CAR, MOTORCYCLE, BICYCLE, or TRUCK
#' @param vehicleDescription Description of vehicle
#' @param vehicleLicensePlate License Plate
#' @param vehicleColor Color
#' @param capacity Maximum number of units this worker can carry
#' @seealso \url{http://docs.onfleet.com/docs/workers} \url{http://docs.onfleet.com/docs/entities#worker}
#' @export
onfleet_post_workers <- function(name, phone, teams, vehicleType = NULL, vehicleDescription = NULL, vehicleLicensePlate = NULL, vehicleColor = NULL, capacity = NULL) {
  if (!is.null(vehicleType)) {
    stopifnot(vehicleType %in% c("CAR", "MOTORCYCLE", "BICYCLE", "TRUCK"))
  }
  onfleet_call("POST", "workers", body = list(
    name = name, phone = phone, teams = list(teams), vehicle = if (!is.null(vehicleType)) list(type = vehicleType, description = vehicleDescription, licensePlate = vehicleLicensePlate,
                                                                                         color = vehicleColor) else NULL, capacity = capacity
  ))
}

#' @rdname onfleet_post_workers
#' @export
onfleet_get_workers <- function(states) {
  onfleet_call("GET", "workers", query = if (isTruthy(states)) list(states = paste0(states, collapse = ","))) %>% {
    data.frame(
      id = map_chr(., "id"),
      name = map_chr(.,"name"),
      phone = map_chr(., "phone"),
      onDuty = map_lgl(., "onDuty")
    )
  }
}

#' @rdname onfleet_post_workers
#' @param long Longtitude
#' @param lat Latitude
#' @param radius Radius of area around lat/long you want to search for workers
#' @export
onfleet_get_workers_by_location <- function(long, lat, radius) {
  onfleet_call("GET", "workers", id = "location", query = list(
    longtitude = long, latitude = lat, radius = radius
  ))
}

#' @rdname onfleet_post_workers
#' @param id ID of worker
#' @export
onfleet_get_worker <- function(id) {
  onfleet_call("GET", "workers", id = id)
}

#' @rdname onfleet_post_workers
#' @param ... values to update
#' @export
onfleet_put_workers <- function(id, ...) {
  onfleet_call("PUT", "workers", id = id, body = list(...))
}

#' @rdname onfleet_post_workers
#' @export
onfleet_delete_workers <- function(id) {
  onfleet_call("DELETE", "workers", id = id)
}

#' @rdname onfleet_post_workers
#' @export
onfleet_get_worker_schedule <- function(id) {
  onfleet_call("GET", "workers", id = paste0(id, "/schedule"))
}

#' @rdname onfleet_post_workers
#' @export
onfleet_put_worker_schedule <- function(id, entries) {
  onfleet_call("PUT", "workers", id = paste0(id, "/schedule"),
               body = entries)
}
