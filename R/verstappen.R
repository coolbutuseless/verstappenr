

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Did Verstappen finish first in the most recent f1 event?
#'
#' This function queries the 'openf1' API for Max's last position
#' in the most recent f1 session (FP1, FP2, FP3, Qualifying, Sprint or Race)
#' 
#' @return String statement of "Yes" or "No" with country, race event and date.
#' 
#' @import httr
#' @import yyjsonr
#' @importFrom utils tail
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
did_max_win <- function() {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sessions for this Race
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sess_url <- "https://api.openf1.org/v1/sessions?session_key=latest"
  sess_raw <- httr::GET(sess_url)
  sessions <- sess_raw$content |> yyjsonr::read_json_raw() 
  this_session <- sessions[, c('country_name', 'session_key', 'session_name', 'date_start', 'date_end')]
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Positions for a driver
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pos_url <- sprintf("https://api.openf1.org/v1/position?session_key=latest&driver_number=1")
  pos_raw <- httr::GET(pos_url) 
  pos_all <- pos_raw$content |> yyjsonr::read_json_raw() 
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Last position for a driver
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_pos <- subset(pos_all, !is.na(pos_all$position))
  this_pos <- this_pos[order(this_pos$date) ,]
  this_pos <- tail(this_pos, 1)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(identical(this_session$session_key, this_pos$session_key))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return Yes/No response
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (this_pos$position == 1) {
    sprintf("Yes [%s - %s (%s)]", this_session$country_name, this_session$session_name, this_session$date_end)
  } else {
    sprintf("No [%s - %s (%s)]", this_session$country_name, this_session$session_name, this_session$date_end)
  }
}
