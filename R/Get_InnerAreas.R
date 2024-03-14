#' Download the classification of peripheral municipalities
#'
#' @description
#' Retrieves the classification of Italian municipalities into six categories; classes D, E, and F are the so-called internal/inner areas while classes A, B and C are the central areas:
#'
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#'
#' @details
#' Classes are defined according to these criteria; see the methodological note (in Italian) for more detail:
#'
#' \itemize{
#'   \item A - Standalone pole municipalities, the highest degree of centrality;
#'   they are characterised by a thorough and self-sufficient combined endowment of school, health and transport infrastructure,
#'    i.e. there are at least a lyceum and a technical high school; a railway station of medium dimensions and a hospital provided with an emergency ward.
#'   \item B - Intermunicipality poles; the endowment of such infrastructures is complete if a small set of contiguous municipalities is considered
#' }
#' The remaining classes are defined in terms of the national distribution of the road distances from a municipality to the closest pole:
#' \itemize{
#'   \item C - Belt municipalities, travel time below the median (< 27'42'') .
#'   \item D - Intermediate municipalities, travel time between the median and the third quartile (27'42'' - 40'54'').
#'   \item E - Peripheral municipalities, travel time between the third quartile and 97.5th percentile (40'54'' - 1h 6' 54'').
#'   \item F - Ultra-peripheral municipalities, travel time over the 97.5th percentile (>1h 6' 54'').
#' }

#'
#'
#' @examples
#'
#'
#' InnerAreas <- Get_InnerAreas()
#'
#' InnerAreas[, c(1,9,13)]
#'
#' @source  <https://www.istat.it/it/archivio/273176>
#' @references <https://www.istat.it/it/files//2022/07/FOCUS-AREE-INTERNE-2021.pdf> (ISTAT Methodological note)
#'
#'
#' @export

Get_InnerAreas <- function(){

  # This version requires readxl. Contact the developer for the version not requiring it.
  Check_connection()

  starttime <- Sys.time()

  home.ISTAT <-"https://www.istat.it/it/archivio/273176"
  homepage <- xml2::read_html(home.ISTAT)
  name_pattern <- "Elenco_Comuni_Classi_di_Aree_Interne"
  link <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% unique()
  link <- grep(name_pattern, link, value = TRUE)


  temp <- tempdir()
  if(!dir.exists(temp)){
    dir.create(temp)
  }

  status <- 0
  while(status != 200){
    base.url <- dirname(home.ISTAT)
    file.url <- xml2::url_absolute(link, base.url)
    response <- httr::GET(file.url, httr::write_disk(tempfile(fileext = ".xlsx")),  httr::config(timeout = 60))
    status <- response$status_code
  }

  excel <- response$request$output$path
  suppressMessages(
    res <- readxl::read_excel(excel, col_names = NA, skip = 3))

  names(res) <-
    c("Municipality_code", "Municipality_code_numeric", "Cadastral_code", "Region_code",
      "Region_description", "Province_code", "Province_initials", "Province_description", "Municipality_description",
      "Inner_area_code_2014_2020", "Inner_area_description_2014_2020",
      "Inner_area_code_2021_2027", "Inner_area_description_2021_2027",
      "Destination_municipality_code", "Destination_municipality_description",
      "Destination_pole_code")
  res$Province_code <- as.numeric(substr(res$Municipality_code, 1, 3))

  if(dir.exists(temp)){
    temp.contents <- list.files(temp, full.names = T)
    for(i in (1:length(temp.contents))) {
      unlink(temp.contents[i], recursive = TRUE)
    }
  }
  return(res)
}


