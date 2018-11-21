
lpsn_names_one_genus <- function(genus) {

  glue("  {genus}") %>% print

  genus_lower <- str_to_lower(genus)
  genus_cap <- str_to_title(genus)
  url <- glue("http://www.bacterio.net/{genus_lower}.html")

  lines <-
    xml2::read_html(url) %>%
    rvest::html_nodes("div#main-text") %>%
    rvest::html_text() %>%
    str_replace_all("Etymology", "\r\nEtymology") %>%
    readr::read_lines() %>%
    str_squish() %>%
    purrr::keep(~ . != "")

  if (lines[[1]] == "File not found") {

    warning(glue("genus {genus} not found"), call. = F)
    return(NULL)

  }

  species_lines <-
    which(str_detect(lines, paste0("^", genus_cap)))
  lines_no_intro <-
    lines[rep(c(F, T), times = c(species_lines[1] - 1, length(lines) - species_lines[1] - 1))]
  species_lines <- species_lines - species_lines[1] + 1
  reps <- diff(c(species_lines, length(lines_no_intro) + 1))

  names <-
    tibble(line = lines_no_intro, name_id = rep(1:length(reps), times = reps)) %>%
    mutate_at("line", str_replace, paste0("^", genus_cap), paste0("name:", genus_cap)) %>%
    mutate_at("line", str_replace, "→ ¤.*", "moved_to:") %>%
    tidyr::separate("line", into = c("field", "value"), sep = ":", extra = "merge", fill = "right") %>%
    mutate_at("field", str_replace, "Sequence accession no\\..*", "type_amplicon") %>%
    mutate_at("field", str_replace, "Type strain", "type_strain_name") %>%
    filter(field %in% c("name", "type_strain_name", "type_amplicon", "moved_to")) %>%
    group_by(name_id, field) %>%
    slice(1) %>%
    ungroup() %>%
    tidyr::spread(key = "field", value = "value") %>%
    purrr::when(
      "moved_to" %in% names(.) ~ .,
      ~ mutate(., moved_to = NA)
    ) %>%
    mutate(no_longer_valid = ! is.na(moved_to)) %>%
    select(- moved_to) %>%
    mutate_at("type_strain_name", str_remove, "\\(see also StrainInfo.net\\)") %>%
    slice(2:n()) %>%
    mutate(species = str_extract(name, "^[^ ]+ [^ ]+")) %>%
    mutate(subspecies = str_extract(name, "subsp\\. [^ ]+")) %>%
    mutate(name = str_c(species, str_replace_na(subspecies, ""), sep = " ")) %>%
    mutate_at("name", str_trim) %>%
    mutate_at("type_amplicon", ~ str_trim(.) %>% str_remove("\\.$")) %>%
    filter(! no_longer_valid) %>%
    select(- no_longer_valid, - name_id)

  names %>%
    group_by(name) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(species = str_extract(name, "^[^ ]+ [^ ]+")) %>%
    tidyr::separate_rows(type_strain_name, sep = "=") %>%
    mutate_at(
      "type_strain_name",
      ~ str_remove(., "\\([^()]*\\)") %>%
        str_remove("\\([^()]*\\)") %>%
        str_trim() %>%
        str_remove("\\.$") %>%
        str_remove("^StrainInfo\\.net\\)")
    ) %>%
    distinct() %>%
    group_by(name, species, type_amplicon) %>%
    summarize(type_strain_name = list(type_strain_name)) %>%
    ungroup() %>%
    mutate_at("type_amplicon", str_extract, "[A-Z0-9]+") %>%
    correct_subspecies() %>%
    summarize_names()

}

lpsn_names <- function(genera) {

  genera %>%
    as.list() %>%
    purrr::map_df(lpsn_names_one_genus) %>%
    bind_rows()

}

# depricated: make lpsn_names() output a table with type_strain_name as nested column
lpsn_tables <- function(genera) {

  names <- lpsn_names(genera)

  type_strain_names <-
    names %>%
    select(name, type_strain_names) %>%
    tidyr::separate_rows(type_strain_names, sep = "=") %>%
    rename(type_strain_name = type_strain_names) %>%
    mutate(comment = str_extract(type_strain_name, "(?<=\\()[^()]*(?=\\))")) %>%
    mutate_at("type_strain_name", str_remove, "\\([^()]*\\)") %>%
    mutate(comment_bis = str_extract(type_strain_name, "(?<=\\()[^()]*(?=\\))")) %>%
    mutate_at("type_strain_name", str_remove, "\\([^()]*\\)") %>%
    mutate_at("type_strain_name", ~ str_trim(.) %>% str_remove("\\.$")) %>%
    mutate_at("type_strain_name", str_remove, "^StrainInfo\\.net\\) ") %>%
    distinct() %>%
    filter(type_strain_name != "")

  names <- select(names, - type_strain_names)

  list(lpsn_names = names, lpsn_type_strain_names = type_strain_names)

}
