#' read_UWL_SR_DAC_SUBJECT_IR_ALL_reports_URL
#'
#' @param manifest_file URL of manifest file
#'
#' @returns data.frame of all files read in manifest_file
#'
read_UWL_SR_DAC_SUBJECT_IR_ALL_reports_URL <- function(manifest_file) {
  manifest <- readr::read_csv(manifest_file) %>%
    dplyr::filter(!is.na(URL)) %>%
    dplyr::select(URL, date)

  enrl <- purrr::map2(
    manifest$URL,
    manifest$date,
    \(u, d) {
      readr::read_csv(u, guess_max = 10000) %>%
        dplyr::mutate(`Enrl Date` = as.Date(d))
    }
  ) %>%
    vctrs::list_rbind()
}

#' read_UWL_SR_DAC_SUBJECT_IR_ALL_reports_locally
#'
#' @param path
#' @param file_pattern
#'
#' @returns data.frame of all files read in path of file_pattern format
#'
read_UWL_SR_DAC_SUBJECT_IR_ALL_reports_locally <- function(path, file_pattern) {
  list.files(
    path = path,
    pattern = paste0("^", file_pattern, "_.*\\.csv$"),
    full.names = TRUE
  ) %>%
    set_names() %>%
    purrr::map_dfr(~ {
      file_name <- basename(.x)
      date <- stringr::str_extract(file_name, "\\d{4}-\\d{2}-\\d{2}")
      readr::read_csv(.x, guess_max = 10000) %>%
        dplyr::mutate(`Enrl Date` = as.Date(date))
    })
}

#' clean_enrollment_data
#'
#' @param enrollment_data data.frame of raw enrollment data
#'
#' @returns data.frame of imported, and cleaned, enrollment data
#'
clean_enrollment_data <- function(raw_enrollment_data) {
  raw_enrollment_data %>%
    format_columns %>%
    dplyr::mutate(
      acad.group = dplyr::case_when(
        acad.group %in% c("CLS", "VPA") ~ "CASSH",
        acad.group == "SAH" ~ "CSH",
        TRUE ~ acad.group
      )
    ) %>%
    tidyr::unite("inst.name", instr.f.name, inst.l.name, sep = " ", na.rm = TRUE) %>%
    dplyr::relocate(inst.name, .before = class.stat) %>%
    remove_excluded_programs_and_components() %>%
    combine_duplicated_sections() %>%
    remove_associated_labs() %>%
    combine_slash_courses() %>%
    determine_minimum_enrollment() %>%
    add_department_info() %>%
    add_gen_ed_categories()
}

#' read_enrollment_data
#'
#' @param path
#' @param file_pattern
#'
#' @returns data.frame of imported, and cleaned, enrollment data
#' @export
#'
read_enrollment_data <- function(path, file_pattern) {
  read_UWL_SR_DAC_SUBJECT_IR_ALL_reports_locally(path, file_pattern) %>%
    clean_enrollment_data
}

#' read_pre_processed_enrollment_data
#'
#' @param file_name
#'
#' @returns data.frame of imported, and cleaned, enrollment data from feather
#' @export
#'
read_pre_processed_enrollment_data <- function(file_name) {
  arrow::read_feather(file_name) %>% clean_enrollment_data
}

#' read_enrollment_data_from_web
#'
#' @param manifest_file URL
#'
#' @returns data.frame of imported, and cleaned, enrollment data
#' @export
#'
read_enrollment_data_from_web <- function(manifest_file) {
  read_UWL_SR_DAC_SUBJECT_IR_ALL_reports_URL(manifest_file) %>%
    clean_enrollment_data
}
