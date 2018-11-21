
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

genbank_sequence <- function(accession, max_attempts = 3) {

  fasta <- NULL
  attempt <- 1

  while(is.null(fasta) && attempt <= max_attempts) {

    print(paste0("attempt ", attempt))

    try ({

      search_res <- rentrez::entrez_search(db = "nucleotide", term = accession)
      fasta <- rentrez::entrez_fetch(db = "nucleotide", id = search_res$ids, rettype = "fasta")

    })

    attempt <- attempt + 1

  }

  fasta %>%
    str_remove(">.*\n")

}
