#' Manage Recipients
#' @param name Recipient name
#' @param phone Recipient phone number
#' @param notes Notes
#' @param skipSMSNotifications Indiate if recipient should not receive notifications
#' @param skipPhoneNumberValidation Indicate if you would like API not to validate phone number
#' @seealso \url{http://docs.onfleet.com/docs/recipients} \url{http://docs.onfleet.com/docs/entities#recipient}
#' @export
onfleet_post_recipients <- function(name, phone, notes = NULL, skipSMSNotifications = NULL,
                                    skipPhoneNumberValidation = NULL) {
  onfleet_call("POST", "recipients", body = list(name = name, phone = phone, notes = notes,
                                                 skipSMSNotifications = skipSMSNotifications,
                                                 skipPhoneNumberValidation = skipPhoneNumberValidation))
}

#' @rdname onfleet_post_recipients
#' @param id ID of recipients
#' @param ... values to update
#' @export
onfleet_put_recipients <- function(id, ...) {
  onfleet_call("PUT", "recipients", id = id, body = list(...))
}

#' @rdname onfleet_post_recipients
#' @export
onfleet_find_recipients <- function(name = NULL, phone = NULL) {
  stopifnot(!(length(name) == 1 && length(phone) == 1))
  onfleet_call("GET", "recipients", id = if (length(name) == 1) paste0("name/", name) else paste0("phone/", phone))
}

#' @rdname onfleet_post_recipients
#' @export
onfleet_get_recipient <- function(id) {
  onfleet_call("GET", "recipients", id = id)
}
