
# names should be a tibble that contains the columns name and species
correct_subspecies <- function(names) {

  names %>%
    mutate(name_is_subspecies = str_detect(name, "(?<=subsp\\. )[^ ]+")) %>%
    group_by(species) %>%
    mutate(species_has_subspecies = any(str_detect(name, "subsp\\."))) %>%
    ungroup() %>%
    mutate(name = if_else(
      species_has_subspecies & ! name_is_subspecies,
      str_c(name, " subsp. ", str_extract(name, "[^ ]+$")),
      name
    )) %>%
    select(- name_is_subspecies, - species_has_subspecies)

}

# names should be a tibble that contains the columns name and species
summarize_names <- function(names) {

  names %>%
    group_by(name, species) %>%
    summarize_all(
      .funs = list(
        ~ . %>%
          as.list() %>%
          purrr::flatten_chr() %>%
          unique() %>%
          list()
      )
    ) %>%
    ungroup()

}
