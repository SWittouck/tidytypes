library(tidytypes)

test_that("function lpsn_names works", {

  expect_equal(
    {
      names <- lpsn_names("Pediococcus")
      names[names$name == "Pediococcus acidilactici", "type_amplicon"][[1]]
    },
    "AF404733"
  )

})
