#' @keywords internal
#'
School.order <- function(data, field = "School_code"){
  res <- data %>% dplyr::mutate(
    Order = ifelse(substr(!!rlang::sym(field),3,4) == "EE", "Primary", ifelse(
      substr(!!rlang::sym(field), 3, 4) == "MM", "Middle", ifelse(
        substr(!!rlang::sym(field), 3,4) %in%
          c ("PC","PM","PQ","PS","RA","RC","RE","RF","RH","RI","RM","RS","RV","SD",
             "SL","TA","TB","TD","TE","TF","TH","TL","TN"), "High", ifelse(
               substr(!!rlang::sym(field),3,4) == "IC", "IC",  ifelse(
                 substr(!!rlang::sym(field),3,4) == "IS", "IS", "NR")))))) %>% dplyr::relocate(.data$Order, .after = field)
  return(res)
}























