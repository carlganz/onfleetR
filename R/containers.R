#' Manage Containers
#' @export
onfleet_put_insert_tasks <- function(containerType, entityId, index, tasks) {
  onfleet_call("PUT", "containers", id = paste0(containerType, "/", entityId),
               body = list(tasks = lapply(c(list(index), tasks),
                                          jsonlite::unbox)))
}

#' @rdname onfleet_put_insert_tasks
#' @export
onfleet_put_container <- function(containerType, entityId, tasks) {
  containerType <- match.arg(containerType, c("organizations", "teams", "workers"))
  onfleet_call("PUT", "containers", id = paste0(containerType, "/", entityId),
               body = list(tasks = tasks))
}