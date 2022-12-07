library(magrittr
        )

make_yml <- function(missing_line, filename = "test.yml") {

  yml <- list(
    name = "CARS",
    output_dir = "",
    output = list(html_document = list(css = "style.css")),
    navbar = list(title = "Title",
      left = list(list(text = "page1", href = "test.html"),
                  list(text = "page2", href = "html"))
    )
  )

  if (!missing(missing_line)) {
    yml <- yml$navbar[-missing_line]
  }

  yaml::write_yaml(yml, filename)
}

test_that("error is raised if there are missing settings", {
  for (i in c(1,2)) {
    make_yml(missing_line = i)
    expect_error(read_site_yml("test.yml"))
    unlink("test.yml")
  }
})

test_that("correct values are returned", {
  make_yml()
  yml <- read_site_yml("test.yml")
  expect_named(yml, c("title", "pages"))
  unlink("test.yml")
})
