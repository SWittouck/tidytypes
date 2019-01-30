
bd_name <- function(name) {

  glue("  {name}") %>% print()

  name <-
    name %>%
    str_to_lower() %>%
    str_replace_all(" ", "+")

  tryCatch(

    {

      species_url_suffix <-
        glue("https://bacdive.dsmz.de/advsearch?advsearch=search&site=advsearch&searchparams%5B73%5D%5Bcontenttype%5D=text&searchparams%5B73%5D%5Btypecontent%5D=exact&searchparams%5B73%5D%5Bsearchterm%5D={name}&searchparams%5B5%5D%5Bsearchterm%5D=1") %>%
        read_html() %>%
        html_node("li.searchresultrow1") %>%
        html_node("a") %>%
        html_attr("href")

      text <-
        glue("https://bacdive.dsmz.de{species_url_suffix}") %>%
        read_html() %>%
        html_nodes("p.infobox_key") %>%
        html_text()

      name <-
        text %>%
        keep(str_detect, "Species") %>%
        str_remove("Species: ") %>%
        purrr::pluck(1) %>%
        when(is.null(.) ~ character(), ~ .)

      strain_designations <-
        text %>%
        keep(str_detect, "Strain Designation: ") %>%
        str_remove("Strain Designation: ") %>%
        str_split(pattern = ", ") %>%
        purrr::pluck(1)

      type_strain_names <-
        text %>%
        keep(str_detect, "Culture col\\. no\\.: ") %>%
        str_remove("Culture col\\. no\\.: ") %>%
        str_split(pattern = ", ") %>%
        purrr::pluck(1) %>%
        c(strain_designations) %>%
        when(is.null(.) ~ as.character(NA), ~ .)

      list(name = name, type_strain_names = c(type_strain_names, strain_designations))

    },

    error = function(e) {
      list(name = as.character(NA), type_strain_names = as.character(NA))

    }

  )

}

bd_synonyms_one_strain_name <- function(strain_name) {

  strain_name <- str_replace_all(strain_name, " ", "+")

  url <- glue("https://bacdive.dsmz.de/search?search={strain_name}&submit=")

  tryCatch(
    read_html(url) %>%
      html_nodes("p.infobox_key") %>%
      html_text() %>%
      keep(str_detect, "Culture col\\. no\\.") %>%
      str_remove("Culture col\\. no\\.: ") %>%
      str_split(pattern = ", ") %>%
      purrr::pluck(1) %>%
      when(is.null(.) ~ character(), ~ .),
    error = function(e) {NA}
  )

}

bd_synonyms <- function(strain_names) {

  strain_names <- keep(strain_names, str_detect, "[a-zA-Z]")

  for (strain_name in strain_names) {
    glue("  trying strain name {strain_name}") %>% print
    syns <- bd_synonyms_one_strain_name(strain_name)
    if (length(syns) != 0 && ! is.na(syns) && ! is.null(syns)) {
      glue("  succes") %>% print
      break
    }
  }

  syns

}
