use_package("dplyr")
use_package("magrittr")

#' determine_minimum_enrollment
#'
#' @param df
#'
#' @returns df data.frame with added column min.enrl
#' @export
#'
determine_minimum_enrollment <- function(df) {
  df %>%
    dplyr::mutate(
      min.enrl = dplyr::case_when(
        acad.group == "CASSH" & catalog.num < 300 ~ 15,
        acad.group == "CASSH" & (catalog.num >= 300 & catalog.num < 500) ~ 12,
        acad.group == "CASSH" & catalog.num >= 500 ~ 0,
        acad.group == "CSH" & course.type %in% c("UGRAD", "SLASH") ~ 10,
        acad.group == "CSH" & course.type == "GRAD" ~ 5,
        TRUE ~ 0
      )
    ) %>%
    dplyr::relocate(min.enrl, .before = tot.enrl)
}

#' get_latest_low_enrolled_classes
#'
#' @param df
#'
#' @returns data.frame with only classes below the minimum enrollment
#' @export
#'
get_latest_low_enrolled_classes <- function(df) {
  df %>%
    dplyr::filter(
      enrl.date == max(enrl.date, na.rm = TRUE),
      tot.enrl < min.enrl
    )
}
