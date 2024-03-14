#' Download the number of teachers in Italian schools by province
#'
#' @description  This functions downloads the number of teachers by province from the open website of the Italian Ministry of Education, University and Research.
#'
#' @param Year Numeric or character value. Reference school year for the school registry data (last available is 2023).
#' Available in the formats: \code{2022}, \code{"2021/2022"}, \code{202122}, \code{20212022.} \code{2023} by default
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param show_col_types Logical. If \code{TRUE}, if the 'verbose' argument is also \code{TRUE}, the columns of the raw dataset are shown during the download. \code{FALSE} by default.
#' @param filename Character. Which data to retrieve among the province counts of teachers/school personnel.
#' By default it is \code{c("DOCTIT", "DOCSUP")}, which are the file names used so far for the number of tenured and temporary teachers respectively.
#' Other file names are the following:
#'
#'
#' \code{"ATATIT"} for the number of tenured non-teaching personnel
#'
#'
#' \code{"ATASUP"} for the number of temporary non-teaching personnel
#'
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#'
#' @source \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Personale+Scuola}{Homepage}
#'
#' @examples
#'
#'
#' nteachers23 <- Get_nteachers_prov(2023, filename = "DOCTIT")
#' nteachers23[, c(3,4,5)]
#'
#'
#' @export
#'
#'
Get_nteachers_prov <- function(Year = 2023, verbose = TRUE, show_col_types = FALSE,
                               filename = c("DOCTIT", "DOCSUP")){

  Check_connection()
  options(dplyr.summarise.inform = FALSE)

  start.zero <- Sys.time()

  home.url <- "https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Personale%20Scuola"
  homepage <- xml2::read_html(home.url)
  name_pattern <- "([0-9]+)\\.(csv)$"
  pattern <- ifelse(year.patternA(Year)=="201516", "1516", year.patternA(Year) )

  links <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% unique()
  links <- links[which(!is.na(links))]
  if (!any(stringr::str_detect(links, pattern))){
    warning("No data available for this year. We apologise for the inconvenience")
    return(NULL)
  }

  files_to_download <- c()
  for (string in links[grep(".csv", links)] ){
    num_numeric_digits <- sum(unlist(gregexpr("[0-9]", string) ) > 0)
    if (num_numeric_digits >= nchar(pattern)){
      chrpart <- gsub("csv", "", gsub("[^[:alpha:]]", "", string))
      numpart <- gsub("[[:alpha:]]", "", string)
      if (!is.na(numpart) & grepl(pattern, numpart) & !string %in% files_to_download &
          any(stringr::str_detect(chrpart, filename))){
        files_to_download <- append(files_to_download, string)
        cat("Found ", string, " as element ",length(files_to_download), "\n")
      }
    }
  }

  base.url <- dirname(home.url)

  input.nteachers <- list()

  starttime <- Sys.time()
  for (link in files_to_download) {
    file.url <- file.path(base.url, link)
    response <- httr::GET(file.url)

    if (httr::http_type(response) %in% c("application/csv", "text/csv", "application/octet-stream")) {
      dat <- readr::read_csv(rawToChar(response$content), show_col_types = show_col_types)
      if(verbose) cat("CSV file downloaded:", link, " ... ")
      element.name <- substr(link,1, regexpr("[0-9]", link)-1)
      input.nteachers[[element.name]] <- dat
      input.nteachers[[element.name]] <- input.nteachers[[element.name]][!duplicated(input.nteachers[[element.name]]),]


    } else {
      warning(paste("Wrong file type:", httr::http_type(response)) )
      cat("Failed to download and process:", link, "\n")
    }
    endtime <- Sys.time()
    if(verbose){
      cat(round(difftime(endtime, starttime, units="secs") ,2), " seconds required \n")
    }
    starttime <- Sys.time()
  }


  if("DOCTIT" %in% filename){
    input.nteachers$DOCTIT <- input.nteachers$DOCTIT %>%
      dplyr::group_by(.data$PROVINCIA, .data$ORDINESCUOLA) %>% dplyr::summarise(
        Tot_teachers = sum(.data$DOCENTITITOLARIFEMMINE) + sum(.data$DOCENTITITOLARIMASCHI)) %>% dplyr::ungroup()

  }
  if("DOCSUP" %in% filename){
    input.nteachers$DOCSUP <- input.nteachers$DOCSUP %>%
      dplyr::group_by(.data$PROVINCIA, .data$ORDINESCUOLA) %>% dplyr::summarise(
        Tot_teachers = sum(.data$DOCENTISUPPLENTIFEMMINE) + sum(.data$DOCENTISUPPLENTIMASCHI)) %>% dplyr::ungroup()
  }

  if("ATATIT" %in% filename){
    input.nteachers$ATATIT <- input.nteachers$ATATIT %>%
      dplyr::group_by(.data$PROVINCIA, .data$ORDINESCUOLA) %>% dplyr::summarise(
        Tot_ATA = sum(.data$ATATITOLARIFEMMINE) + sum(.data$ATATITOLARIMASCHI)) %>% dplyr::ungroup()

  }
  if("ATASUP" %in% filename){
    input.nteachers$ATASUP <- input.nteachers$ATASUP %>%
      dplyr::group_by(.data$PROVINCIA, .data$ORDINESCUOLA) %>% dplyr::summarise(
        Tot_ATA = sum(.data$ATASUPPLENTIFEMMINE) + sum(.data$ATASUPPLENTIMASCHI)) %>% dplyr::ungroup()
  }


  npers.Tot <- input.nteachers[[1]]
  if (length(input.nteachers)>1){
    for(i in (2:length(input.nteachers))){
      names(input.nteachers[[i]]) <- names(npers.Tot)
      npers.Tot <- rbind(npers.Tot, input.nteachers[[i]])
    }
  }


  if("Tot_teachers" %in% names(npers.Tot)){
    npers.Tot <- npers.Tot %>% dplyr::group_by(.data$PROVINCIA, .data$ORDINESCUOLA) %>%
      dplyr::summarise(Tot_teachers = sum(.data$Tot_teachers)) %>% dplyr::ungroup()
  } else if ("Tot_ATA" %in% names(npers.Tot)){
    npers.Tot <- npers.Tot %>% dplyr::group_by(.data$PROVINCIA, .data$ORDINESCUOLA) %>%
      dplyr::summarise(Tot_ATA = sum(.data$Tot_ATA)) %>% dplyr::ungroup()
  }
  npers.Tot <- npers.Tot %>% rename_by_idx(c(1,2), c("Province_description", "Order")) %>%
    dplyr::filter(.data$Order != "SCUOLA INFANZIA")

  npers.Tot$Order <- npers.Tot$Order %>%
    stringr::str_replace_all("SCUOLA PRIMARIA", "Primary") %>%
    stringr::str_replace_all("SCUOLA SECONDARIA I GRADO", "Middle") %>%
    stringr::str_replace_all("SCUOLA SECONDARIA II GRADO", "High")


  names(npers.Tot) <- stringr::str_to_title(names(npers.Tot))

  npers.Tot$Province_description <- stringr::str_to_title(npers.Tot$Province_description) %>%
    stringr::str_replace_all("Monza E Brianza", "Monza E Della Brianza") %>%
    stringr::str_replace_all("Barletta-Adria-Trani", "Barletta-Andria-Trani") %>%
    stringr::str_replace_all("Medio-Campitano", "Medio Campidano") %>%
    stringr::str_replace_all("Pesaro-Urbino", "Pesaro E Urbino")
  npers.Tot$Province_description[which(npers.Tot$Province_description=="Massa")] <- "Massa-Carrara"

  provnames <- prov.names() %>% dplyr::select(-.data$Region_code, -.data$Region_description) %>%
    dplyr::mutate(dplyr::across(.data$Province_description, ~ stringr::str_to_title(.)))


  npers.Tot <- npers.Tot %>% dplyr::left_join(provnames, by = "Province_description") %>%
      dplyr::relocate(.data$Order, .after = "Province_initials") %>%
      dplyr::relocate(.data$Tot_teachers, .after = "Order")


  return(npers.Tot)

}

