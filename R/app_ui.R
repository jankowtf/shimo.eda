#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    shiny::tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        shinydashboardPlus::dashboardPage(
            # title = "Demo",
            skin = "red",
            options = list(
                sidebarExpandOnHover = TRUE
            ),
            header = shinydashboardPlus::dashboardHeader(
                title = "Demo",
                titleWidth = 100,
                fixed = TRUE,

                leftUi = htmltools::tagList(
                )
            ),
            sidebar = shinydashboardPlus::dashboardSidebar(
                id = "sidebar",
                width = 300,
                shinydashboard::sidebarMenu(
                    id = "tabs",

                    # --- Select ---
                    shinydashboard::menuItem(
                        "Select",
                        icon = icon("filter"),
                        startExpanded = TRUE,
                        shinydashboard::menuSubItem("Separate boxes",
                            tabName = "select", icon = icon("arrow-right")),
                        shinydashboard::menuSubItem("Enclosing box",
                            tabName = "select_outer", icon = icon("arrow-right"))
                    ),
                    shinyjs::hidden(
                        shinydashboard::menuItem("select_hidden",
                            tabName = "select_hidden", selected = TRUE)
                    ),

                    # --- Freq table ---
                    shinydashboard::menuItem(
                        "Frequency table",
                        icon = icon("table"),
                        shinydashboard::menuSubItem("Separate boxes",
                            tabName = "freq_table", icon = icon("arrow-right")),
                        shinydashboard::menuSubItem("Enclosing box",
                            tabName = "freq_table_outer", icon = icon("arrow-right"))
                    ),
                    shinyjs::hidden(
                        shinydashboard::menuItem("freq_table_hidden",
                            tabName = "freq_table_hidden")
                    )
                )
            ),
            body = shinydashboard::dashboardBody(
                shinyjs::useShinyjs(),
                shinydashboard::tabItems(
                    # --- Select ---
                    shinydashboard::tabItem(
                        tabName = "select_hidden",
                        vertical_space(2),
                        h3("Select (click subitem on the left)")
                    ),
                    shinydashboard::tabItem(
                        tabName = "select",
                        vertical_space(2),
                        h3("Select"),
                        mod_eda_select_ui(verbose = TRUE)
                    ),
                    shinydashboard::tabItem(
                        tabName = "select_outer",
                        vertical_space(2),
                        h3("Select with outer box"),
                        mod_eda_select_ui(id = "select_outer", outer_box = TRUE, verbose = TRUE)
                    ),

                    # --- Freq table ---
                    shinydashboard::tabItem(
                        tabName = "freq_table_hidden",
                        vertical_space(2),
                        h3("Frequency table (click subitem on the left)")
                    ),
                    shinydashboard::tabItem(
                        tabName = "freq_table",
                        vertical_space(2),
                        h3("Frequency table"),
                        mod_eda_freq_table_ui(id = "freq_table", verbose = TRUE)
                    ),
                    shinydashboard::tabItem(
                        tabName = "freq_table_outer",
                        vertical_space(2),
                        h3("Frequency table with outer box"),
                        mod_eda_freq_table_ui(id = "freq_table_outer", outer_box = TRUE, verbose = TRUE)
                    )
                )
            ),
            controlbar = shinydashboardPlus::dashboardControlbar()
        )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shimo.eda"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
