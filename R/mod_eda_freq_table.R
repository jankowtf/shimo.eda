# UI ----------------------------------------------------------------------

#' export UI Function
#'
#' @param id [[character]] Module ID
#' @param grouping_title
#' @param grouping_width
#' @param grouping_button_label
#' @param grouping_button_class
#' @param grouping_button_icon
#' @param grouping_button_width
#' @param freq.box_title
#' @param outer_box [[logical]] Wrap in outer box yes/no
#' @param outer_title
#' @param outer_width
#' @param freq.box_width
#' @param freq.box_title_width
#' @param freq.dropdown_title
#' @param freq.dropdown_width
#' @param freq.dropdown_col_n_abs_label
#' @param freq.dropdown_col_n_rel_label
#' @param freq.col_n_abs
#' @param freq.col_n_rel
#' @param verbose [[logical]] Tracing infos yes/no
#'
#' @description A shiny Module.
#'
#' @importFrom shiny NS tagList
#' @export
mod_eda_freq_table_ui <- function(
    id = NULL,
    # --- Grouping
    grouping_title = "Group data",
    grouping_width = 12,
    grouping_button_label = "Add group-by statement",
    grouping_button_class = "btn-primary",
    # grouping_button_style = "background: #0c3992;color: #fffff;",
    grouping_button_icon = icon('plus'),
    grouping_button_width = 190,
    # --- Freq table
    freq.box_title = "Frequency table",
    freq.box_width = 12,
    freq.box_title_width= 200,
    freq.dropdown_title = "Column names",
    freq.dropdown_width= 150,
    freq.dropdown_col_n_abs_label = "Name for abs. count",
    freq.dropdown_col_n_rel_label = "Name for rel. count",
    freq.col_n_abs = "n_abs",
    freq.col_n_rel = "n_rel",
    # --- Outer
    outer_box = FALSE,
    outer_title = "Frequency table",
    outer_width = 12,
    verbose = FALSE
) {
    ns <- NS(id)

    shiny_trace_ns_ui(
        fn_name = "mod_eda_freq_table_ui",
        id_inner = "foo",
        ns = ns,
        verbose = verbose
    )

    shiny::selectInput("test", label = NULL, choices = letters)

    ui <- tagList(
        fluidRow(
            shinydashboardPlus::box(
                title = grouping_title,
                width = grouping_width,
                collapsible = TRUE,
                actionButton(
                    ns("add_group_by"),
                    label = grouping_button_label,
                    class = grouping_button_class,
                    # style = grouping_button_style,
                    icon = grouping_button_icon,
                    width = grouping_button_width
                ),
                vertical_space(2),
                column(
                    width = 3,
                    uiOutput(ns("grouping_ui"))
                )
                # sidebar = shinydashboardPlus::boxSidebar(
                #     id = ns("boxsidebar"),
                #     width = 25,
                #     startOpen = TRUE
                #     # textInput(ns("col_n_abs"), label = NULL,
                #     #     value = freq.col_n_abs),
                #     # textInput(ns("col_n_rel"), label = NULL,
                #     #     value = freq.col_n_rel)
                # )
            )
        ),
        fluidRow(
            shinydashboardPlus::box(
                # title = freq.box_title,
                title = div(
                    div(
                        style = "display: inline-block;vertical-align:middle;width:{freq.box_title_width};" %>%
                            stringr::str_glue(),
                        h4(freq.box_title)
                    ),
                    shiny::div(
                        style = "display: inline-block;vertical-align:middle;width:{freq.dropdown_width}px;" %>%
                            stringr::str_glue(),
                        shinyWidgets::dropdownButton(
                            h4(freq.dropdown_title),
                            textInput(ns("col_n_abs"),
                                label = freq.dropdown_col_n_abs_label,
                                value = freq.col_n_abs),
                            textInput(ns("col_n_rel"),
                                label = freq.dropdown_col_n_rel_label,
                                value = freq.col_n_rel),
                            circle = TRUE,
                            size = "xs",
                            status = "danger",
                            icon = icon("gear"),
                            width = "150px",
                            tooltip = shinyWidgets::tooltipOptions(
                                title = "Click to see inputs")
                        )
                    )
                ),
                width = freq.box_width,
                collapsible = TRUE,

                vertical_space(1),
                DT::DTOutput(ns("grouping_tbl"))
            ),
        ),
        # sortable::sortable_js(ns("group_by_ui")),
        tags$script(src = "shimo.eda.js"),
        tags$script(paste0("shimo_eda_mod_freq_table_js('", ns(''), "')"))
    )

    if (outer_box) {
        fluidRow(
            shinydashboardPlus::box(
                title = tags$b(outer_title),
                width = outer_width,
                collapsible = TRUE,
                ui
            )
        )
    } else {
        ui
    }
}

# Server ------------------------------------------------------------------

#' export Server Function
#'
#' @param id [[character]] Module ID
#' @param r_data [[reactive]] Reactive function that serves data
#' @param dt_bundle_buttons [[function]] Seet [[dtf::dt_bundle_buttons]]
#' @param dt_bundle_internationalization [[function]] Seet [[dtf::dt_bundle_internationalization]]
#' @param verbose [[logical]] Tracing infos yes/no
#'
#' @export
mod_eda_freq_table_server <- function(
    id = NULL,
    r_data,
    dt_bundle_buttons = dtf::dt_bundle_buttons_en,
    dt_bundle_internationalization = dtf::dt_bundle_internationalization_en,
    verbose = FALSE
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # --- Create UI ---
        input_ids <- get_input_ids(input_id_prefix = "grouping_input", sort = TRUE)
        input_values <- get_input_values(input_ids = input_ids)

        shiny_trace_ns_server(
            fn_name = "mod_eda_freq_table_server",
            id_inner = input_ids,
            verbose = verbose
        )

        create_group_by_ui <- create_group_by_ui(
            r_data = r_data,
            input_ids = input_ids,
            input_values = input_values
        )

        render_grouping_ui(
            create_ui = create_group_by_ui
        )

        # --- Remove UI ---
        remove_grouping_ui(id = NULL)

        # --- Render data table ---
        render_grouping_data_table(
            id = NULL,
            r_data = r_data,
            input_ids = input_ids,
            input_values = input_values,
            dt_bundle_buttons = dt_bundle_buttons,
            dt_bundle_internationalization = dt_bundle_internationalization
        )
    })
}

# Create inputs -----------------------------------------------------------

create_group_by_ui <- function(
    id = NULL,
    r_data,
    input_ids,
    input_values,
    input_id_prefix = "grouping_input"
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        eventReactive(input$add_group_by, {
            # browser()
            cols <- r_data() %>% names()

            # --- Get input IDs and values ---
            input_ids <- input_ids()
            input_values <- input_values()

            # --- Handle existing select inputs ---
            select_existing <- handle_existing_grouping_inputs(
                cols = cols,
                input_values = input_values
            )

            # --- Handle columns
            cols <- r_data %>% handle_col_values(input_values = input_values)

            # --- Compose UI elements ---
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
        }, ignoreInit = FALSE)
    })
}

# Remove inputs -----------------------------------------------------------

remove_grouping_ui <- function(
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
                select_id <- button_id %>% derive_input_id_from_button_id(
                    button_prefix = "del_"
                )

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

# Handle existing inputs --------------------------------------------------

handle_existing_grouping_inputs <- function(
    id = NULL,
    cols,
    input_values = list(),
    input_id_prefix = "grouping_input"
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

# Render UI ---------------------------------------------------------------

render_grouping_ui <- function(
    id = NULL,
    create_ui,
    output_id = "grouping_ui"
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output[[output_id]] <- renderUI({
            create_ui()
        })
    })
}

# Render data table -------------------------------------------------------

#' Title
#'
#' @param id
#' @param r_data
#' @param input_ids
#' @param input_values
#' @param output_id
#' @param freq_tab.col_n_abs
#' @param freq_tab.col_n_rel
#' @param freq_tab.sort
#' @param dt_bundle_buttons
#' @param dt_bundle_internationalization
#'
#' @return
#' @export
#'
#' @examples
render_grouping_data_table <- function(
    id = NULL,
    r_data,
    input_ids,
    input_values,
    output_id = "grouping_tbl",
    freq_tab.col_n_abs = "col_n_abs",
    freq_tab.col_n_rel = "col_n_rel",
    freq_tab.sort = TRUE,
    dt_bundle_buttons = dtf::dt_bundle_buttons_en,
    dt_bundle_internationalization = dtf::dt_bundle_internationalization_en
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # --- Transform
        r_data_trans <- reactive({
            data <- r_data()
            .col_n_abs <- input$col_n_abs
            .col_n_rel <-  input$col_n_rel
            # browser()

            input_ids <- input_ids()
            if (length(input_ids)) {
                cols <- input_values() %>% unname() %>% dplyr::syms()
                if (length(cols)) {
                    data %>%
                        wrang::wr_freq_table(
                            !!!cols,
                            .col_n_abs = rlang::eval_bare(.col_n_abs),
                            .col_n_rel = rlang::eval_tidy(.col_n_rel),
                            .sort = freq_tab.sort
                        )
                } else {
                    data
                }
            } else {
                data
            }
        })

        # --- Render
        dtf::mod_render_dt_server(
            id = id,
            output_id = output_id,
            data = r_data_trans,
            scrollY = 300,
            left = 1,
            .bundles = list(
                dt_bundle_buttons(),
                dt_bundle_internationalization()
            )
        )
    })
}
