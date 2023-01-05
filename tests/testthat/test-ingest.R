
test_that("tidy_colnames reformats headers and removes first 2 rows", {

  dummy_data <- data.frame(Q1. = c(NA, NA, "test1", "test2"),
                           Q2. = c("Q2.1.", NA, "test1", "test2"),
                           Q3. = c("Q3.1.", "X.1", "test1", "test2")
  )

  got <- tidy_colnames(dummy_data)

  exp <- data.frame(Q1. = c("test1", "test2"),
                    "Q2.:Q2.1." = c("test1", "test2"),
                    "Q3.:Q3.1." = c("test1", "test2"),
                    check.names = FALSE
  )

  expect_equal(got, exp)

})


test_that("convert_raw reads raw string as a dataframe and converts missing value strings", {

  r <- list(status_code = 200,
            content = charToRaw("col1,col2\r\n
test,11\r\n
,.\r\n
NA,-\r\n
\"\",\".\"\r\n
\"NA\",\"-\"")
  )

  class(r) <- "response"

  got <- convert_raw(r)

  exp <- data.frame(col1 = c("test", rep(NA, 4)),
                    col2 = c("11", rep(NA, 4))
  )

  expect_equal(got, exp)

})

