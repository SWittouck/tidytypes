
si_name <- function(name) {

  glue("  {name}") %>% print()

  name <-
    name %>%
    str_to_lower() %>%
    str_replace_all(" ", "+")

  tryCatch(

    {

      url <- glue("http://www.straininfo.net/taxa/search?speciesName={name}")
      url <- httr::HEAD(url)$url

      page <-
        xml2::read_html(url)

      name <-
        page %>%
        rvest::html_nodes("h1") %>%
        rvest::html_nodes("span.speciesname") %>%
        rvest::html_text() %>%
        purrr::when(length(.) == 0 ~ as.character(NA), ~ .)

      type_strain_names <-
        page %>%
        rvest::html_nodes("div.strainnumber") %>%
        rvest::html_text() %>%
        str_squish() %>%
        str_remove(" T .*$") %>%
        purrr::when(length(.) == 0 ~ as.character(NA), ~ .)

      type_sixteen_s <-
        page %>%
        rvest::html_nodes("div.modulecontent") %>%
        rvest::html_nodes("table") %>%
        purrr::pluck(1) %>%
        rvest::html_table(fill = T) %>%
        filter(X1 == "16S rRNA gene") %>%
        pull(X2)

      list(name = name, type_strain_names = type_strain_names, type_sixteen_s = type_sixteen_s)

    },

    error = function(e) {
      list(name = as.character(NA), type_strain_names = as.character(NA))
    }

  )

}

si_synonyms_one_strain_name <- function(strain_name) {

  strain_name <- str_replace_all(strain_name, " ", "+")

  url <- glue("http://www.straininfo.net/strains/search?strainNumber={strain_name}")
  url <- httr::HEAD(url)$url

  tryCatch(
    read_html(url) %>%
      rvest::html_nodes("div.strainnumber") %>%
      rvest::html_text() %>%
      str_squish() %>%
      str_remove(" T .*$"),
    error = function(e) {NA}
  )

}

si_synonyms <- function(strain_names) {

  strain_names <- purrr::keep(strain_names, str_detect, "[a-zA-Z]")

  for (strain_name in strain_names) {
    glue("  trying strain name {strain_name}") %>% print
    syns <- si_synonyms_one_strain_name(strain_name)
    if (length(syns) != 0 && ! is.na(syns)) {
      glue("  succes") %>% print
      break
    }
  }

  syns

}

# under construction --> rework to function si_names

straininfo_parsed_node <- function(node) {

  list(
    species_names =
      html_nodes(node, ".speciesname") %>%
      html_text() %>%
      unique,
    strain_names =
      html_nodes(node, ".strainnumber") %>%
      html_text() %>%
      str_extract("(?<=\n\t\t)[^\n]+") %>%
      unique()
  )

}

straininfo_list_one_page <- function(genus, pagenumber) {

  genus <- str_to_lower(genus)
  first_result <- (pagenumber - 1) * 10 + 1

  url <- glue("http://www.straininfo.net/strains/search?typeStrain=true&taxon={genus}&includeSubtaxa=true&firstResult={first_result}")

  xml2::read_html(url) %>%
    rvest::html_nodes("li") %>%
    keep(~ rvest::html_text(.) %>% str_detect("species name")) %>%
    map(straininfo_parsed_node)

}

straininfo_list_one_genus <- function(genus) {

  genus <- "Leuconostoc"

  genus <- str_to_lower(genus)

  url_first_page <- glue("http://www.straininfo.net/strains/search?typeStrain=true&taxon={genus}&includeSubtaxa=true&firstResult=1")

  last_page <-
    xml2::read_html(url_first_page) %>%
    rvest::html_nodes("ul.pagination-clean") %>%
    rvest::html_text() %>%
    str_squish() %>%
    nth(1) %>%
    str_extract("[^ ]+(?= next)") %>%
    as.integer()

  res <-
    1:last_page %>%
    map(~ straininfo_list_one_page(genus, .)) %>%
    flatten()

}

straininfo_tables <- function(genera) {

  species_list <-
    genera %>%
    map(straininfo_list_one_genus) %>%
    flatten()

  results <-
    tibble(
      species_names = map(species_list, "species_names"),
      strain_names = map(species_list, "strain_names")
    ) %>%
    mutate(name_id = str_c("name", 1:n(), sep = " "))

  straininfo_tables <- list()

  straininfo_tables$species_names <-
    results %>%
    unnest(species_names) %>%
    rename(name = species_names) %>%
    mutate_at("name", str_remove, " subsp\\.")

  straininfo_tables$type_strain_names <-
    results %>%
    unnest(strain_names) %>%
    rename(strain_name = strain_names) %>%
    distinct()

  straininfo_tables

}
