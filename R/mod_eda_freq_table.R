#' export UI Function
#'
#' @param id [[character]] Module ID
#'
#' @description A shiny Module.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_eda_freq_table_ui <- function(
    id = NULL,
    title_group_by = "Group by",
    width_group_by = 12,
    label_group_by_button = "Add group by",
    title_freq_table = "Frequency table",
    width_freq_table = 12
) {
    ns <- NS(id)

    shiny::selectInput("test", label = NULL, choices = letters)

    tagList(
        # column(
        #     width = 3,
        #     actionButton(
        #         ns("add_group_by"),
        #         "Add group by",
        #         class = "btn-success",
        #         style = "color: #fff;",
        #         icon = icon('plus'),
        #         width = '100%'
        #     ),
        #     tags$br(),
        #     tags$br()
        # ),
        fluidRow(
            # column(
            shinydashboardPlus::box(
                width = width_group_by,
                title = title_group_by,
                collapsible = TRUE,
                actionButton(
                    ns("add_group_by"),
                    label = label_group_by_button,
                    class = "btn-success",
                    style = "color: #fff;",
                    icon = icon('plus'),
                    width = 150
                ),
                tags$br(),
                tags$br(),
                uiOutput(ns("group_by_ui"))
            )
        ),
        fluidRow(
            # column(
            shinydashboardPlus::box(
                width = width_freq_table,
                title = title_freq_table,
                collapsible = TRUE,
                DT::DTOutput(ns("group_by_tbl"))
            )
        ),
        # sortable::sortable_js(ns("group_by_ui")),
        tags$script(src = "shimo.eda.js"),
        tags$script(paste0("shimo_eda_mod_freq_table_js('", ns(''), "')"))
    )
}

#' export Server Function
#'
#' @param id [[character]] Module ID
#'
#' @noRd
mod_eda_freq_table_server <- function(
    id = NULL,
    r_data
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$sortable_group_by_ui <- renderPrint({
            input$group_by_ui
        })

        input_ids <- get_input_ids(input_id_prefix = "group_by_input", sort = TRUE)
        input_values <- get_input_values(input_ids = input_ids)

        create_group_by_ui <- create_group_by_ui(
            r_data = r_data,
            input_ids = input_ids,
            input_values = input_values
        )

        # Add group_by UI
        output$group_by_ui <- renderUI({
            create_group_by_ui()
        })

        # Remove group_by UIO
        remove_group_by_ui(id = NULL)

        output$group_by_tbl <- DT::renderDT({
            data <- r_data()

            group_by_ids <- input_ids()
            if (length(group_by_ids)) {
                cols <- input_values() %>% unname() %>% dplyr::syms()
                if (length(cols)) {

                    data %>%
                        wrang::wr_freq_table(!!!cols)
                } else {
                    data
                }
            } else {
                data
            }
        })
    })
}

# Helper functions --------------------------------------------------------

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

create_group_by_ui <- function(
    id = NULL,
    r_data,
    input_ids,
    input_values,
    input_id_prefix = "group_by_input"
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        eventReactive(input$add_group_by, {
            # browser()
            choices <- input %>% names() %>% stringr::str_subset("^group_by_input")
            names <- r_data() %>% names()

            cols <- r_data() %>% names()

            input_ids <- input_ids()
            input_values <- input_values()

            select_existing <- handle_existing_group_by_inputs(
                cols = cols,
                input_values = input_values
            )

            cols <- if (length(input_values)) {
                names[!(names %in% input_values)]
            } else {
                names
            }

            pos_latest <- length(select_existing) + 1

            input_id <- ns("{input_id_prefix}_{pos_latest}" %>% stringr::str_glue())

            select_new <- div(
                div(style = "display:inline-block; vertical-align:top; width:200px;",
                    selectInput(
                        inputId = input_id,
                        label = NULL,
                        choices = cols,
                        width = 200
                    )
                ),
                div(style = "display:inline-block; vertical-align:top; width:40px;",
                    actionButton(
                        inputId = ns("del_{input_id_prefix}_{pos_latest}" %>% stringr::str_glue()),
                        label = NULL,
                        # icon = icon("times"),
                        icon = icon("trash-alt"),
                        width = 40,
                        class = "btn-danger delete_btn",
                        # `data-toggle` = "tooltip",
                        # `data-placement` = "top",
                        title = "Delete"
                    )
                ),
                id =input_id
            )

            # id_del_button <- ns("del_{input_id_prefix}_{pos_latest}" %>% stringr::str_glue())
            # --- Approach 1
            # Via plain vanilla JS
            # -> yes

            # --- Approach 2
            # shinyjs::onclick(id_del_button,
            #     shinyjs::html(ns("time"), date()))
            # shinyjs::onclick(id_del_button,
            #     shinyjs::html(ns("ui_to_delete_id"), input_id))
            # -> no

            # --- Approach 3
            # ns_prefix <- ns('')
            # shinyjs::onclick(id_del_button,
            #     shinyjs::runjs('Shiny.setInputValue(ns_prefix + "ui_to_delete_id", input_id, { priority: "event"});')
            # )
            # -> no

            # --- Approach 4
            # shinyjs::onclick(id_del_button,
            #     updateTextInput(inputId = ns("ui_to_delete_id"), value = input_id)
            # )
            # -> yes

            tagList(
                select_existing,
                select_new
            )
        }, ignoreInit = TRUE)
    })
}

remove_group_by_ui <- function(
    id = NULL
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        observeEvent(input$ui_to_delete_id, {
            # browser()
            button_id <- input$ui_to_delete_id

            if (length(button_id) &&
                    button_id != ""
            ) {
                select_id <- button_id %>% stringr::str_remove("^del_")

                # --- Approach 1: imperative removal
                # if (select_id %in% names(ui_existing)) {
                #     ui_existing[select_id] <- NULL
                # }
                # -> nope

                # --- Approach 2: declarative removal
                removeUI(
                    selector = "#{select_id}" %>% stringr::str_glue()
                )
                shinyjs::runjs('Shiny.onInputChange("{select_id}", null)' %>%
                        stringr::str_glue())
                # -> yes
            }
        }, ignoreInit = TRUE)
    })
}

handle_existing_group_by_inputs <- function(
    id = NULL,
    cols,
    input_values = list(),
    input_id_prefix = "group_by_input"
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        if (length(input_values)) {
            input_values %>%
                unname() %>%
                purrr::imap(function(.input, .pos) {
                    input_id <- ns("{input_id_prefix}_{.pos}" %>% stringr::str_glue())
                    div(
                        id = input_id,
                        div(style = "display:inline-block; vertical-align:top; width:200px;",
                            selectInput(
                                inputId = input_id,
                                label = NULL,
                                choices = cols,
                                selected = .input,
                                width = 200
                            )
                        ),
                        div(style = "display:inline-block; vertical-align:top; width:40px;",
                            actionButton(
                                inputId = ns("del_{input_id_prefix}_{.pos}" %>% stringr::str_glue()),
                                label = NULL,
                                # icon = icon("times"),
                                icon = icon("trash-alt"),
                                width = 40,
                                class = "btn-danger delete_btn",
                                # `data-toggle` = "tooltip",
                                # `data-placement` = "top",
                                title = "Delete"
                            )
                        )
                    )
                }) %>%
                purrr::set_names(names(input_values))
        } else {
            NULL
        }
    })
}
