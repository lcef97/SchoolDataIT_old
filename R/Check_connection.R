#' @keywords internal
#'
Check_connection <- function(){
  while(!curl::has_internet()){
    message("There seems to be no internet connection. Would you wait or abort the operation? \n",
            "    - To abort the operation, press `A`\n",
            "    - To hold on for 10 seconds, press any other key\n")
    holdOn <- readline(prompt = "    ")
    if (toupper(holdOn) == "A") {
      stop("You chose to abort the operation. Please try later \n")
    } else {
      cat("You chose to wait 10 seconds \n")
      Sys.sleep(10)
    }
  }
}
