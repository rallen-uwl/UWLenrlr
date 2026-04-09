#' determine_minimum_enrollment
#'
#' @param df data.frame
#'
#' @returns data.frame original data.frame with added column min.enrl
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
      ),
      percent.fill = if_else(min.enrl == 0, 0, round((tot.enrl/min.enrl)*100, digits = 0))
    ) %>%
    dplyr::relocate(min.enrl, .before = tot.enrl) %>%
    dplyr::relocate(percent.fill, .before = tot.enrl)
}

#' get_latest_low_enrolled_classes
#'
#' @param df data.frame
#'
#' @returns data.frame only classes below the minimum enrollment
#' @export
#'
get_latest_low_enrolled_classes <- function(df) {
  df %>%
    dplyr::filter(
      enrl.date == max(enrl.date, na.rm = TRUE),
      tot.enrl < min.enrl
    )
}
