#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)

# Get input IDs -----------------------------------------------------------

get_input_ids <- function(
    input_id_prefix = "group_by_input",
    input_id_pattern = "^{input_id_prefix}" %>% stringr::str_glue(),
    sort = FALSE,
    .id = NULL
) {
    shiny::moduleServer(.id, function(input, output, session) {
        ns <- session$ns
        reactive({
            input %>%
                names() %>%
                stringr::str_subset(input_id_pattern) %>% {
                    if (sort) {
                        sort(.)
                    } else {
                        .
                    }
                }
        })
    })
}

# Get input values --------------------------------------------------------

get_input_values <- function(
    input_ids,
    sort = FALSE,
    .id = NULL
) {
    shiny::moduleServer(.id, function(input, output, session) {
        ns <- session$ns
        reactive({
            input_ids <- input_ids()
            if (length(input_ids)) {
                input_ids %>% {
                    if (sort) {
                        sort(.)
                    } else {
                        .
                    }
                } %>%
                    purrr::map(~input[[.]]) %>%
                    purrr::set_names(input_ids) %>%
                    drop::drop_null() %>% # Very important! That's why we use 'map()' instead of 'map_chr()'
                    unlist()
            } else {
                NULL
            }
        })
    })
}

# Handle column values ----------------------------------------------------------

handle_col_values <- function(
    data,
    input_values = list(),
    .id = NULL
) {
    shiny::moduleServer(.id, function(input, output, session) {
        ns <- session$ns

        names <- data() %>% names()

        if (length(input_values)) {
            names[!(names %in% input_values)]
        } else {
            names
        }
    })
}

# Derive input ID from button ID ------------------------------------------

derive_input_id_from_button_id <- function(
    button_id,
    button_prefix = "del_",
    .id = NULL
) {
    shiny::moduleServer(.id, function(input, output, session) {
        ns <- session$ns

        button_id %>%
            stringr::str_remove(
                "(?<=^{ns('')}){button_prefix}" %>% stringr::str_glue())
    })
}

# Vertical space ----------------------------------------------------------

#' Add vertical space
#'
#' Useful when using [shinyDashboard](https://github.com/rstudio/shinydashboard)
#'
#' @param times [[integer]] Number of `tag$br()` to set
#'
#' @return
#' @export
#'
#' @examples
#' vertical_space()
#' vertical_space(2)
vertical_space <- function(times = 1) {
    1:times %>% purrr::map(~tags$br()) %>%
        tagList()
}
