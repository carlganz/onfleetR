#' Manage Tasks
#' @param merchant Organization ID of merchant
#' @param executor Organization ID of executor
#' @param destination ID of destination
#' @param recipients Vector of recipient IDs
#' @param completeAfter Timestamp
#' @param completeBefore Timestamp
#' @param pickupTask Indicate if pickup
#' @param dependencies Vector of tasks this task depends on 
#' @param notes Notes
#@param autoAssign
#@param container
#' @param quantity Number of units dropped off
#' @param serviceTime Time spent at destination
#' @seealso \url{http://docs.onfleet.com/docs/tasks} \url{http://docs.onfleet.com/docs/entities#task}
#' @export
onfleet_post_tasks <- function(merchant = NULL, executor = NULL, destination, recipients,
                               completeAfter= NULL, completeBefore= NULL, pickupTask= NULL,
                               dependencies= NULL, notes= NULL, 
                               autoAssignMode= NULL, autoAssignTeam= NULL, autoAssignMaxAssignedTaskCount= NULL,
                               autoAssignConsiderDependencie= NULLs, autoAssignExcludedWorkerIds= NULL,
                               containerType= NULL, containerTeam= NULL, containerWorker= NULL,
                               quantity= NULL, serviceTime= NULL) {
  
  body <- list(
    container = if (!is.null(containerType)) list(type = containerType, team = containerTeam, worker = containerWorker),
    autoAssign = if (!is.null(autoAssignMode)) list(mode = autoAssignMode, team = autoAssignTeam, maxAssignedTaskCount = autoAssignMaxAssignedTaskCount,
                                                    considerDependencies = autoAssignConsiderDependencies, excludeWorkerIds = autoAssignExcludedWorkerIds),
    merchant = merchant, executor = executor, destination = destination, recipients = recipients,
    completeAfter = completeAfter, completeBefore = completeBefore, pickupTask = pickupTask, 
    dependencies = dependencies, notes = notes, quantity = quantity, serviceTime = serviceTime
  )
  
  onfleet_call("POST", "tasks", body = shiny:::dropNullsOrEmpty(body))
  
}

#' @rdname onfleet_post_tasks
#' @export
onfleet_get_tasks <- function(from, to = NULL, lastId = NULL, state = NULL, worker = NULL, completeBeforeBefore = NULL,
                              completeAfterAfter = NULL) {
  onfleet_call("GET", "tasks/all", query = list(
    from = as.numeric(as.POSIXct(from))*1000, to = if (!is.null(to)) as.numeric(as.POSIXct(to))*1000, lastId = lastId, state = if (isTruthy(state)) paste0(state, collapse = ","), 
    worker = worker, 
    completeBeforeBefore = completeBeforeBefore,
    completeAfterAfter = completeAfterAfter
  ))
}

#' @rdname onfleet_post_tasks
#' @export
onfleet_get_task <- function(id) {
  onfleet_call("GET", "tasks", id = id)
}

#' @rdname onfleet_post_tasks
#' @export
onfleet_put_task <- function(id, ...) {
  onfleet_call("PUT", "tasks", id = id, body = list(...))
}

#' @rdname onfleet_post_tasks
#' @export
onfleet_post_complete_task <- function(id, success, notes) {
  onfleet_call("POST", "tasks", id = paste0(id, "/complete"),
               body = list(completionDetails = 
                             list(success = success, 
                                  notes = notes)))
}

#' @rdname onfleet_post_tasks
#' @export
onfleet_delete_task <- function(id) {
  onfleet_call("DELETE", "tasks", id = id)
}

