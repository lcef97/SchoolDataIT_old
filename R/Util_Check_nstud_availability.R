#' Check how many schools in the school registries are included in the students count dataframe
#'
#' @description  This function checks for which schools listed in the two registries (the one from the buildings section and the proper one from the buildings section)
#' the count of students is available. The first registry is indicated as \code{Registry1} and the second one as \code{Registry2}.
#'
#'
#'
#'
#'
#'
#'
#'
#' @param data Object of class  \code{tbl_df}, \code{tbl} and \code{data.frame}, obtained as output of the \code{\link{Util_nstud_wide}} function
#' @param Year Numeric or character value. Reference school year.
#' Available in the formats: \code{2023}, \code{"2022/2023"}, \code{202223}, \code{20222023}.
#' @param cutout Character. The types of schools not to be taken into account (because not relevant or because they are out of scope in the students number section). By default \code{c("IC", "IS", "NR")} , i.e. the check does not regard comprehensive institutes, superior institutes, and all the schools that cannot be classified either as primary, middle or high schools.
#' @param ggplot Logical. If \code{TRUE}, the function displays a static map of the availability of the students number by province (but it does not save the ggplot object into the global environment). \code{TRUE} by default.
#' @param toplot_registry Character. If the \code{ggplot} option is chosen, the students number availability of which registry must be plotted; either \code{"Registry1"}, \code{"Registry2"}, \code{"Any"} or \code{"Both"}. \code{"Any"} by default.
#' @param InnerAreas Logical. Whether it must be checked if municipalities belong to inner areas or not. \code{TRUE} by default.
#' @param ord_InnerAreas Logical. Whether the inner areas classification should be treated as an ordinal variable rather than as a categorical one (see \code{\link{Get_InnerAreas}} for the classification).
#' \code{FALSE} by default.
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param input_Registry2 Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, obtained as output of the function \code{\link{Get_Registry}}
#' The school registry from the registry section.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default
#' @param input_InnerAreas Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#' The classification of peripheral municipalities, obtained as output of the \code{\link{Get_InnerAreas}} function.
#' Needed only if the \code{InnerAreas} option is chosen.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment.
#' \code{NULL} by default
#' @param input_Prov_shp Object of class \code{sf}, \code{tbl_df}, \code{tbl}, \code{data.frame}. The relevant shapefile of Italian municipalities, if the \code{ggplot} option is chosen.
#'  If \code{NULL} it is downloaded automatically but not saved in the global environment. \code{NULL} by default.
#' @param input_AdmUnNames Object of class \code{tbl_df}, \code{tbl} and \code{data.frame}, obtained as output of the function \code{\link{Get_AdmUnNames}}
#' The ISTAT file including all the codes and the names of the administrative units for the year in scope.
#' Only needed if the argument input_School2mun is \code{NULL} and has to be computed.
#' If \code{NULL}, it will be downloaded automatically, but not saved in the global environment. \code{NULL} by default.
#' @param input_School2mun Object of class \code{list} with elements of class \code{tbl_df}, \code{tbl} and \code{data.frame}, obtained as output of the function \code{\link{Get_School2mun}}.
#' The mapping from school codes to municipality (and province) codes. If \code{NULL}, it will be downloaded automatically, but not saved in the global environment. \code{NULL} by default.
#'
#'
#'
#'
#' @return An object of class \code{list} including two elements:
#' \itemize{
#'   \item \code{$Municipality_data}
#'   \item \code{$Province_data}
#' }
#' Both the elements are objects of class \code{list} including four elements:
#'
#' \itemize{
#'   \item \code{$Registry1}: object of class of class \code{tbl_df}, \code{tbl} and \code{data.frame}: the availability of the number of students in the schools listed in the buildings section.
#'   \item \code{$Registry2}: object of class of class \code{tbl_df}, \code{tbl} and \code{data.frame}: the availability of the number of students in the schools listed in the registry section.
#'   \item \code{$Any}: object of class of class \code{tbl_df}, \code{tbl} and \code{data.frame}: the availability of the number of students in the schools listed anywhere.
#'   \item \code{$Both}: object of class of class \code{tbl_df}, \code{tbl} and \code{data.frame}: the availability of the number of students in the schools listed in both sections.
#' }
#'
#' @references \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/leaf/?area=Edilizia+Scolastica&datasetId=DS0101EDIANAGRAFESTA2021}{Buildings Registry};
#' \href{https://dati.istruzione.it/opendata/opendata/catalogo/elements1/leaf/?area=Scuole&datasetId=DS0400SCUANAGRAFESTAT}{Schools Registry}
#'
#'
#'
#'
#' @examples
#'
#'
#' data("example_input_nstud23")
#' data("example_input_Registry23")
#' data("example_School2mun23")
#' data("example_Prov22_shp")
#'
#'
#'
#' nstud23 <- Util_nstud_wide(example_input_nstud23, verbose = FALSE)
#'
#' Util_Check_nstud_availability(nstud23, Year = 2023,
#'   input_Registry2 = example_input_Registry23, InnerAreas = FALSE,
#'   input_School2mun = example_School2mun23, input_Prov_shp = example_Prov22_shp)
#'
#'
#'
#' @import sf
#'
#' @export



Util_Check_nstud_availability <- function(data, Year,
                                          cutout = c("IC", "IS", "NR"), verbose = TRUE,
                                          ggplot = TRUE, toplot_registry = "Any",
                                          InnerAreas = TRUE, ord_InnerAreas = FALSE,
                                          input_Registry2 = NULL, input_InnerAreas = NULL,
                                          input_Prov_shp = NULL, input_AdmUnNames = NULL, input_School2mun = NULL){

  options(dplyr.summarise.inform = FALSE)

  if(is.null(input_Prov_shp) & ggplot){
    if(verbose) cat("Downloading the shapefile (since ggplot has been required) \n")
    input_Prov_shp <- Get_Shapefile(
      Year = as.numeric(year.patternA(Year))%/%100+1,level = "NUTS-3", lightShp = TRUE)
  }

  if(is.null(input_School2mun)){
    input_School2mun <- Get_School2mun(Year = Year, verbose = verbose,
                                       input_AdmUnNames = input_AdmUnNames, input_Registry2 = input_Registry2)
  }

  if(InnerAreas){
    if(is.null(input_InnerAreas)){
      if(verbose) cat("Retrieving the classification of inner areas \n")
      input_InnerAreas <- Get_InnerAreas()
    }

    if(dplyr::between(as.numeric(substr(year.patternA(Year),1,4))+1, 2021, 2027)){
      InnerAreas.R <- input_InnerAreas %>%
        dplyr::select(.data$Municipality_code, .data$Inner_area_code_2021_2027) %>%
        dplyr::mutate(Inner_area = ifelse(
          .data$Inner_area_code_2021_2027 %in% c("A", "B", "C"), 0, 1)) %>%
        dplyr::rename(Inner_area_code = .data$Inner_area_code_2021_2027)
    } else {
      InnerAreas.R <- input_InnerAreas %>%
        dplyr::select(.data$Municipality_code, .data$Inner_area_code_2014_2020) %>%
        dplyr::mutate(Inner_area = ifelse(
          .data$Inner_area_code_2014_2020 %in% c("A", "B", "C"), 0, 1)) %>%
        dplyr::rename(Inner_area_code = .data$Inner_area_code_2014_2020)
    }

    if(ord_InnerAreas){
      InnerAreas.R <- InnerAreas.R %>% dplyr::mutate(
        A_mun = as.numeric(.data$Inner_area_code == "A"),
        B_mun = as.numeric(.data$Inner_area_code == "B"),
        C_mun = as.numeric(.data$Inner_area_code == "C"),
        D_mun = as.numeric(.data$Inner_area_code == "D"),
        E_mun = as.numeric(.data$Inner_area_code == "E"),
        F_mun = as.numeric(.data$Inner_area_code == "F") )
    }
    InnerAreas.R <- InnerAreas.R %>% dplyr::select(-.data$Inner_area_code)
  }

  suppressWarnings(Names.to.find <- data %>% dplyr::select(.data$School_code) %>% unlist() %>% as.vector())

  Schools <- input_School2mun %>% lapply (function(x){
    suppressWarnings(
      x <- x %>%
        dplyr::mutate(Available = as.numeric(.data$School_code %in% Names.to.find)) %>%
        School.order(field = "School_code") %>%
        dplyr::filter(!.data$Order %in% cutout) %>%
        dplyr::filter(!.data$Province_initials %in% c("AO", "BZ", "TN"))
      # Schools in the Valle d'Aosta region have problematic data and are therefore out of scope
    )
    if(InnerAreas) x <- x %>% dplyr::left_join(InnerAreas.R, by = "Municipality_code")
    return(x)
  })

  Municipality_data <- Schools %>% lapply(function(x){
    if(InnerAreas){
      if(ord_InnerAreas){
        suppressWarnings(
          x <- x %>% dplyr::filter(!is.na(.data$Inner_area)) %>%
            dplyr::group_by(.data$Order, .data$Municipality_code, .data$Municipality_description,
                            .data$Province_code, .data$Province_initials) %>%
            dplyr::summarise(
              Available = sum(.data$Available),
              Unavailable = dplyr::n()-sum(.data$Available),
              Inner_area =  mean(.data$Inner_area),
              A_mun = mean(.data$A_mun),
              B_mun = mean(.data$B_mun),
              C_mun = mean(.data$C_mun),
              D_mun = mean(.data$D_mun),
              E_mun = mean(.data$E_mun),
              F_mun = mean(.data$F_mun)) %>%
            dplyr::ungroup () %>%
            dplyr::mutate(Availability = .data$Available/(.data$Available + .data$Unavailable)))
      } else{
        suppressWarnings(
          x <- x %>% dplyr::filter(!is.na(.data$Inner_area)) %>%
            dplyr::group_by(.data$Order, .data$Municipality_code, .data$Municipality_description,
                            .data$Province_code, .data$Province_initials) %>%
            dplyr::summarise(
              Available = sum(.data$Available),
              Unavailable = dplyr::n()-sum(.data$Available),
              Inner_area =  mean(.data$Inner_area)) %>%
            dplyr::ungroup () %>%
            dplyr::mutate(Availability = .data$Available/(.data$Available+.data$Unavailable)))
      }
    } else {
      suppressWarnings(
        x <- x %>% dplyr::group_by(.data$Order, .data$Municipality_code, .data$Municipality_description,
                               .data$Province_code, .data$Province_initials) %>%
          dplyr::summarise(
            Available = sum(.data$Available),
            Unavailable = dplyr::n()-sum(.data$Available)) %>%
          dplyr::ungroup () %>%
          dplyr::mutate(Availability = .data$Available/(.data$Available+.data$Unavailable)))
    }
  })

  Province_data <- Schools %>% lapply( function(x){
    if(InnerAreas){
      if(ord_InnerAreas){
        suppressWarnings(
          x <- x %>% dplyr::filter(!is.na(.data$Inner_area)) %>%
            dplyr::group_by(.data$Order, .data$Province_code, .data$Province_initials) %>%
            dplyr::summarise(
              Available = sum(.data$Available),
              Unavailable = dplyr::n()-sum(.data$Available),
              Inner_area =  mean(.data$Inner_area),
              A_mun = mean(.data$A_mun),
              B_mun = mean(.data$B_mun),
              C_mun = mean(.data$C_mun),
              D_mun = mean(.data$D_mun),
              E_mun = mean(.data$E_mun),
              F_mun = mean(.data$F_mun)) %>%
            dplyr::ungroup () %>%
            dplyr::mutate(Availability = .data$Available/(.data$Available+.data$Unavailable)))
      } else {
        suppressWarnings(
          x <- x %>% dplyr::filter(!is.na(.data$Inner_area)) %>%
            dplyr::group_by(.data$Order, .data$Province_code, .data$Province_initials) %>%
            dplyr::summarise(
              Available = sum(.data$Available),
              Unavailable = dplyr::n()-sum(.data$Available),
              Inner_area =  mean(.data$Inner_area)) %>%
            dplyr::ungroup () %>%
            dplyr::mutate(Availability = .data$Available/(.data$Available+.data$Unavailable)))
      }
    } else{
      suppressWarnings(
        x <- x %>% dplyr::group_by(.data$Order, .data$Province_code, .data$Province_initials) %>%
          dplyr::summarise(
            Available = sum(.data$Available),
            Unavailable = dplyr::n()-sum(.data$Available)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Availability = .data$Available/(.data$Available+.data$Unavailable)))
    }
    return(x)
  })

  if(ggplot){
    while (!toplot_registry %in% names(Municipality_data)){
      message(paste("Please, choose a valid register source to plot among:", paste(names(Municipality_data), collapse = ", ")))
      toplot_registry <- readline(prompt = "    ")
    }

    R.Availability.wide <- Province_data[[toplot_registry]] %>% dplyr::filter(.data$Order %in% c("Primary", "Middle", "High")) %>%
      dplyr::select(.data$Order, .data$Province_code, .data$Availability) %>%
      tidyr::spread(key = .data$Order, value = .data$Availability)
      #tidyr::pivot_wider(id_cols = .data$Province_code, names_from = .data$Order, values_from = .data$Availability)


    dat.plot.long <- input_Prov_shp %>% dplyr::select(.data$COD_PROV) %>%
      dplyr::rename(Province_code = .data$COD_PROV) %>% dplyr::left_join(R.Availability.wide, by = "Province_code") %>%
      tidyr::gather("Order", "Availability", -.data$Province_code, -.data$geometry) #

    toplot <- ggplot2::ggplot(dat.plot.long, ggplot2::aes(fill = .data$Availability)) + ggplot2::geom_sf() +
      ggplot2::facet_wrap(~ .data$Order) + ggplot2::labs(title = paste("% Students number coverage by province in year ", Year))

    plot(toplot)
  }

  return(list(Municipality_data = Municipality_data, Province_data = Province_data))
}
