# UI ----------------------------------------------------------------------

#' export UI Function
#'
#' @param id [[character]] Module ID
#' @param ns [[function]] Namespace function
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
#' @param grouping.button_submit_label
#' @param grouping.button_submit_class
#' @param grouping.button_submit_style
#' @param grouping.button_submit_icon
#' @param grouping.button_submit_width
#'
#' @description A shiny Module.
#'
#' @importFrom shiny NS tagList
#' @export
mod_group_by_ui <- function(
    id = "freq_table",
    ns = function() {},
    # --- Grouping
    grouping_title = "Group by",
    grouping_width = 12,
    grouping_button_label = "Add group-by statement",
    grouping_button_class = "btn-primary",
    # grouping_button_style = "background: #0c3992;color: #fffff;",
    grouping_button_icon = icon('plus'),
    grouping_button_width = 190,
    grouping.button_submit_label = "Submit",
    grouping.button_submit_class = "btn-success",
    grouping.button_submit_style = "color: #fff;",
    grouping.button_submit_icon = icon('check'),
    grouping.button_submit_width = 100,
    # --- Outer
    outer_box = FALSE,
    outer_title = "Frequency table",
    outer_width = 12,
    verbose = FALSE
) {
    if (!missing(ns)) {
        id <- ns(id)
    }

    ns <- NS(id)

    shiny_trace_ns_ui(
        fn_name = "mod_group_by_ui",
        id_inner = "foo",
        ns = ns,
        verbose = verbose
    )

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
                    uiOutput(ns("grouping_ui")),
                    actionButton(
                        ns("submit"),
                        label = grouping.button_submit_label,
                        class = grouping.button_submit_class,
                        # style = grouping.button_submit_style,
                        icon = grouping.button_submit_icon,
                        width = grouping.button_submit_width
                    )
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

        # sortable::sortable_js(ns("group_by_ui")),
        tags$script(src = "shimo.eda.js"),
        tags$script(paste0("shimo_eda_mod_group_by_js('", ns(''), "')"))
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
#' @param data [[reactive]] Reactive function that serves data
#' @param dt_bundle_buttons [[function]] Seet [[dtf::dt_bundle_buttons]]
#' @param dt_bundle_internationalization [[function]] Seet [[dtf::dt_bundle_internationalization]]
#' @param verbose [[logical]] Tracing infos yes/no
#' @param dt_bundles_additional [[list]] Additional DT bundles. See
#'   [dtf::dt_bundle_dom] as an example
#' @param transform_fn
#' @param dt.filter [[character]] DT column filter settings. See package [dtf]
#' @param dt.scroll_y [[integer]] DT: pixels for vertical scrolling
#' @param dt.left [[integer]] DT: columns to fix from the left
#'
#' @export
mod_group_by_server <- function(
    id = "group_by",
    data,
    dt_bundle_buttons = dtf::dt_bundle_buttons_en,
    dt_bundle_internationalization = dtf::dt_bundle_internationalization_en,
    dt_bundles_additional = list(),
    transform_fn = identity,
    dt.filter = dtf::valid_dt_filter_values(1),
    dt.scroll_y = 300,
    dt.left = 1L,
    verbose = FALSE
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # --- Create UI ---
        input_ids <- get_input_ids(input_id_prefix = "grouping_input", sort = TRUE)
        input_values <- get_input_values(input_ids = input_ids)

        shiny_trace_ns_server(
            fn_name = "mod_group_by_server",
            id_inner = input_ids,
            verbose = verbose
        )

        fn_group_by_ui <- create_group_by_ui(
            data = data,
            input_ids = input_ids,
            input_values = input_values
        )

        render_grouping_ui(
            fn = fn_group_by_ui
        )

        # --- Remove UI ---
        remove_grouping_ui(id = NULL)

        # --- Group by ---
        data_grouped <- reactive_group_by(
            id = NULL,
            data = data,
            input_ids = input_ids,
            input_values = input_values
        )

        return(data_grouped)
    })
}

# Create inputs -----------------------------------------------------------

create_group_by_ui <- function(
    id = NULL,
    data,
    input_ids,
    input_values,
    input_id_prefix = "grouping_input"
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        eventReactive(input$add_group_by, {
            # browser()
            cols <- data() %>% names()

            # --- Get input IDs and values ---
            input_ids <- input_ids()
            input_values <- input_values()

            # --- Handle existing select inputs ---
            select_existing <- handle_existing_grouping_inputs(
                cols = cols,
                input_values = input_values
            )

            # --- Handle columns
            cols <- data %>% handle_col_values(input_values = input_values)

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
    fn,
    output_id = "grouping_ui"
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output[[output_id]] <- renderUI({
            fn()
        })
    })
}
