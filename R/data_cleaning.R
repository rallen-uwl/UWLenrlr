#' format_columns
#'
#' @param df
#'
#' @export
#'
format_columns <- function(df) {
  df %>%
    dplyr::select(-dplyr::all_of(dplyr::starts_with("..."))) %>%
    dplyr::select_all(~ gsub("\\s+|\\.|\\..|\\...", ".", .)) %>%
    dplyr::select_all(tolower)
}

#' add_department_info
#'
#' @param df
#'
#' @export
#'
add_department_info <- function(df) {
  df %>%
    dplyr::left_join(lookup_subject_lookup, by = "subject") %>%
    dplyr::left_join(lookup_discipline_lookup, by = "dept") %>%
    dplyr::relocate(dept, .before = subject)
}

#' remove_excluded_programs_and_components
#'
#' @param df
#'
#' @export
#'
remove_excluded_programs_and_components <- function(df) {
  df %>%
    dplyr::filter(
      !(component == "IND"),
      !(subject %in% lookup_excluded_programs)
    )
}

#' combine_duplicated_sections
#'
#' @param df
#'
#' @export
#'
combine_duplicated_sections <- function(df) {
  orig_names <- names(df)

  df %>%
    dplyr::group_by(enrl.date, class.nbr) %>%
    dplyr::summarise(
      inst.name = stringr::str_c(unique(inst.name[!is.na(inst.name)]), collapse = ", "),
      dplyr::across(-inst.name, first),
      .groups = "drop"
    ) %>%
    dplyr::relocate(dplyr::all_of(orig_names))
}

#' remove_associated_labs
#'
#' @param df
#'
#' @export
#'
remove_associated_labs <- function(df) {
  df %>%
    dplyr::group_by(enrl.date, subject, catalog.num) %>%
    dplyr::filter(!(any(component == "LEC") & component %in% c("LAB", "DIS", "FLD"))) %>%
    dplyr::ungroup()
}

#' combine_slash_courses
#'
#' @param df
#'
#' @export
#'
combine_slash_courses <- function(df) {
  orig_names <- names(df)
  updated_names <- append(orig_names, "course.type", after = match("session", orig_names))

  cols_to_keep <-
    dplyr::setdiff(
      updated_names,
      c("enrl.date", "subject", "section", "catalog.num", "class.nbr", "tot.enrl", "inst.name", "course.type")
    )

  df %>%
    dplyr::mutate(
      catalog.num = as.integer(catalog.num),
      base.catalog.num = dplyr::if_else(
        dplyr::between(catalog.num, 500, 599),
        catalog.num - 100L,
        catalog.num
      )
    ) %>%
    dplyr::group_by(enrl.date, subject, base.catalog.num, section) %>%
    dplyr::summarise(
      has_4xx = any(dplyr::between(catalog.num, 400, 499)),
      has_5xx = any(dplyr::between(catalog.num, 500, 599)),

      dplyr::across(dplyr::all_of(cols_to_keep), ~ dplyr::first(.x, default = NA)),

      catalog.num = dplyr::case_when(
        has_4xx & has_5xx ~ paste0(dplyr::first(base.catalog.num), "/", dplyr::first(base.catalog.num) + 100L),
        has_4xx ~ as.character(dplyr::first(catalog.num[catalog.num < 500], default = NA_integer_)),
        has_5xx ~ as.character(dplyr::first(catalog.num[catalog.num >= 500], default = NA_integer_)),
        TRUE ~ as.character(dplyr::first(base.catalog.num))
      ),

      course.type = dplyr::case_when(
        has_4xx & has_5xx ~ "SLASH",
        catalog.num < 500 ~ "UGRAD",
        TRUE              ~ "GRAD"
      ),

      class.nbr = stringr::str_c(sort(unique(class.nbr)), collapse = "/"),
      tot.enrl = sum(tot.enrl, na.rm = TRUE),
      inst.name = dplyr::first(inst.name),
      .groups = "drop"
    ) %>%
    dplyr::select(-base.catalog.num, -has_4xx, -has_5xx) %>%
    dplyr::relocate(dplyr::all_of(updated_names))
}
