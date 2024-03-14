#' Download the boundaries of NUTS-3 (Provinces) and LAU (Municipalities) Italian administrative units from the ISTAT website
#'
#' @description '
#'
#' @param Year Numeric value. Reference year for the administrative units.
#' @param level Character. Either \code{"NUTS-3"}, \code{"Province"}, \code{"LAU"}, \code{"Municipality"}. \code{"LAU"} by default
#' @param lightShp Logical. If \code{TRUE}, the function downloads a generalised, i.e.less detailed, and lighter version of the shapefiles.
#' \code{TRUE} by default
#'
#' @return A spatial data frame of class \code{data.frame} and \code{sf}.
#'
#' @source <https://www.istat.it/it/archivio/222527>
#'
#' @examples
#'
#' \donttest{
#'   library(magrittr)
#'   library(sf)
#'
#'   Prov23_shp <- Get_Shapefile(2023, lightShp = TRUE, level = "NUTS-3")
#'   ggplot2::ggplot() + ggplot2::geom_sf(data = Prov23_shp) +
#'     ggplot2::ggtitle("Italian provinces in 2023/01/01")
#'  }
#'
#'
#'
#'
#'
#'
#' @export


Get_Shapefile <- function(Year, level = "LAU", lightShp = TRUE){

  while(Year < 2001 & Year != 1991){
    message(paste("Year", Year, "not available. Please chose another year between 2001 and the current year"))
    Year <- readline(prompt= "   ")
  }

  Check_connection()

  home.ISTAT.Shp <- "https://www.istat.it/it/archivio/222527"
  homepage <- xml2::read_html(home.ISTAT.Shp)

  links <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% unique()
  links <- grep(paste0("confini_amministrativi.*", Year), links, value = TRUE)
  links.NR <- grep("ED50", links, value = TRUE)
  links <- links[which(!links %in% links.NR)]

  if(lightShp == FALSE){
    link <- grep(paste0(Year, ".zip"),links, value=TRUE)
  } else {
    link <- grep(paste0(Year, "_g.zip"),links, value=TRUE)
  }

  base.url <- dirname(home.ISTAT.Shp)
  file.url <- xml2::url_absolute(link, base.url)

  temp1 <- tempfile()
  temp2 <- tempfile()
  timeout.default <- options("timeout")$timeout
  if(timeout.default < 120) options(timeout = 120)
  utils::download.file(url = file.url, destfile = temp1)
  options(timeout = timeout.default)
  utils::unzip(zipfile = temp1, exdir = temp2)
  files.int <- list.files(list.files(list.files(temp2, full.names = TRUE), full.names = TRUE), full.names = TRUE)

  pattern <- dplyr::case_when(level %in% c("LAU", "Municipality") ~ "Com",
                              level %in% c("NUTS-3", "Province") ~ "Prov",
                              level %in% c("NUTS-2", "Region") ~ "Reg")


  filename.int <- grep(pattern, files.int, value = TRUE)
  filename.int <- filename.int[which(substr(filename.int, nchar(filename.int)-2, nchar(filename.int)) == "shp")]

  res <- sf::read_sf( filename.int)

  unlink(temp1, recursive = TRUE, force = TRUE)
  unlink(temp2, recursive = TRUE, force = TRUE)

  return(res)
}
