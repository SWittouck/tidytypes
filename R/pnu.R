
# returns a named list with information for a valid name
pnu_name <- function(url, username, password) {

  auth <- httr::authenticate(username, password)

  record <-
    url %>%
    httr::GET(auth) %>%
    httr::content()

  glue("found record for {record$species}") %>% print()

  record

}

# returns tibble with info of names
# names that are no longer valid (those with a "correct_name") are removed
pnu_names_one_genus <- function(genus, username, password) {

  auth <- httr::authenticate(username, password)

  genus <- str_to_title(genus)

  record <-
    glue("https://bacdive.dsmz.de/api/pnu/name/{genus}/") %>%
    httr::GET(auth) %>%
    httr::content()

  names <-
    purrr::map_chr(record$species, 1) %>%
    purrr::map(pnu_name, username, password) %>%
    purrr::transpose() %>%
    do.call(what = tibble) %>%
    mutate_all(purrr::simplify, .type = character(1)) %>%
    mutate_all(purrr::simplify, .type = numeric(1)) %>%
    mutate_at("correct_name", purrr::map_chr, 2, .default = NA) %>%
    filter(is.na(correct_name)) %>%
    select(name = species, type_strain_name = type_strain) %>%
    mutate_at("type_strain_name", purrr::map, as.character) %>%
    mutate(species = str_extract(name, "^[^ ]+ [^ ]+"))

  names

}

pnu_names <- function(genera, username, password) {

  auth <- httr::authenticate(username, password)

  genera %>%
    purrr::map_dfr(pnu_names_one_genus, username, password)

}
