test_that("shiny_font_size_perc", {
    result <- shiny_font_size_perc("foo")
    expected <- structure(list(name = "div", attribs = list(style = structure("font-size:75%;", class = c("glue",
        "character"))), children = list("foo")), class = "shiny.tag")
    expect_identical(result, expected)
})

test_that("shiny_font_size_perc: 50", {
    result <- shiny_font_size_perc("foo", 50)
    expected <- structure(list(name = "div", attribs = list(style = structure("font-size:50%;", class = c("glue",
        "character"))), children = list("foo")), class = "shiny.tag")
    expect_identical(result, expected)
})
