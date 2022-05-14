#' export UI Function
#'
#' @param id [[character]] Module ID
#'
#' @description A shiny Module.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @export
mod_eda_select_ui <- function(
    id = "eda_select",
    select_title = "Select columns",
    select_width = 12,
    select_id = "select_ui",
    select_button_label = "Add select statement",
    select_button_class = "btn-success",
    select_button_style = "color: #fff;",
    select_button_icon = icon('plus'),
    select_button_width = 170,
    data_title = "Data table",
    data_width = 12
) {
    ns <- NS(id)

    shiny::selectInput("test", label = NULL, choices = letters)

    tagList(
        fluidRow(
            # column(
            shinydashboardPlus::box(
                title = select_title,
                width = select_width,
                collapsible = TRUE,
                actionButton(
                    ns("add_select"),
                    label = select_button_label,
                    class = select_button_class,
                    style = select_button_style,
                    icon = select_button_icon,
                    width = select_button_width
                ),
                tags$br(),
                tags$br(),
                uiOutput(ns("select_ui"))
            )
        ),
        fluidRow(
            # column(
            shinydashboardPlus::box(
                width = select_width,
                title = select_title,
                collapsible = TRUE,
                DT::DTOutput(ns("select_tbl"))
            )
        ),
        tags$script(src = "shimo.eda.js"),
        tags$script(paste0("shimo_eda_mod_select_js('", ns(''), "')"))
    )
}

#' export Server Function
#'
#' @param id [[character]] Module ID
#'
#' @noRd
#' @export
mod_eda_select_server <- function(
    id = "eda_select",
    r_data,
    input_id_prefix = "select_input"
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        # browser()

        # --- Create select UI ----
        input_ids <- get_input_ids(input_id_prefix = input_id_prefix, sort = TRUE)
        input_values <- get_input_values(input_ids = input_ids)

        create_select_ui <- create_select_ui(
            r_data = r_data,
            input_ids = input_ids,
            input_values = input_values,
            input_id_prefix = input_id_prefix
        )

        # output$select_ui <- renderUI({
        #     create_select_ui()
        # })
        render_select_ui(id = NULL, create_select_ui = create_select_ui)

        # --- Remove select UI ---
        remove_select_ui(id = NULL)

        # --- Render data table ---
        render_select_data_table(
            id = NULL,
            r_data = r_data,
            input_ids = input_ids,
            input_values = input_values
        )
    })
}

# Create inputs -----------------------------------------------------------

create_select_ui <- function(
    id = NULL,
    r_data,
    input_ids,
    input_values,
    input_id_prefix = "select_input",
    input_id_button = "add_select"
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        input_id_pattern <- "^{input_id_prefix}" %>% stringr::str_glue()

        eventReactive(input[[input_id_button]], {
            # browser()
            # --- Get input IDs and values ---
            input_ids <- input_ids()
            input_values <- input_values()

            # --- Handle existing select inputs ---
            cols <- r_data() %>% names()
            select_existing <- handle_existing_select_inputs(
                cols = cols,
                input_values = input_values
            )

            # --- Handle columns
            cols <- r_data %>% handle_col_values(input_values = input_values)

            # --- Compose UI elements ---
            pos_latest <- length(select_existing) + 1

            input_id <- ns("{input_id_prefix}_{pos_latest}" %>% stringr::str_glue())
            # print(input_id)

            select_new <- div(
                id = input_id,
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
                        icon = icon("trash-alt"),
                        width = 40,
                        class = "btn-danger delete_btn",
                        title = "Delete"
                    )
                )
            )

            tagList(
                select_existing,
                select_new
            )
        }, ignoreInit = TRUE)
    })
}

# Remove inputs -----------------------------------------------------------

remove_select_ui <- function(
    id = NULL,
    id_input = "ui_to_delete_id"
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        observeEvent(input[[id_input]], {
            # browser()
            button_id <- input[[id_input]]

            if (length(button_id) &&
                    button_id != ""
            ) {
                select_id <- button_id %>% derive_input_id_from_button_id(
                    button_prefix = "del_"
                )

                removeUI(
                    selector = "#{select_id}" %>% stringr::str_glue()
                )
                shinyjs::runjs('Shiny.onInputChange("{select_id}", null)' %>%
                        stringr::str_glue())
            }
        }, ignoreInit = TRUE)
    })
}

# Handle existing inputs --------------------------------------------------

handle_existing_select_inputs <- function(
    id = NULL,
    cols,
    input_values = list(),
    input_id_prefix = "select_input"
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
                                icon = icon("trash-alt"),
                                width = 40,
                                class = "btn-danger delete_btn",
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

# Render UI ---------------------------------------------------------------

render_select_ui <- function(
    id = NULL,
    create_select_ui,
    output_id = "select_ui"
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output[[output_id]] <- renderUI({
            create_select_ui()
        })
    })
}

# Render data table -------------------------------------------------------

render_select_data_table <- function(
    id = NULL,
    r_data,
    input_ids,
    input_values
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$select_tbl <- DT::renderDT({
            data <- r_data()

            group_by_ids <- input_ids()
            if (length(group_by_ids)) {
                cols <- input_values() %>% unname() %>% dplyr::syms()
                if (length(cols)) {

                    # data %>%
                    #     wrang::wr_freq_table(!!!cols)
                    data %>%
                        dplyr::select(!!!cols)
                } else {
                    data
                }
            } else {
                data
            }
        })
    })
}
