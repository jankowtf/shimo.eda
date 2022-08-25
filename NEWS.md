# shimo.eda 0.0.0.9021 (2022-08-26)

Stand-alone group by

- Modified `mod_eda_freq_table.R`: renamed to `mod_freq_table.R`, added `submit` button, added `reactive_group_by()`
- Modified arguments: renamed `r_data` to `data`
- Added `mod_group_by.R`

----------

# shimo.eda 0.0.0.9020 (2022-08-25)

Selectize

- Modified `mod_eda_select.R`: renamed to `mod_select()`, added `render_data`, added `reactive_select()`
- Added `mod_select2.R` with selectize input

----------

# shimo.eda 0.0.0.9019 (2022-05-31)

'shiny_font_size_perc()'

- Added `shiny_font_size_perc()`: a convenience wrapper around `div(<string>, style = 'font-size:<percent>%;')`

----------

# shimo.eda 0.0.0.9018 (2022-05-31)

Custom DT bundles, filter & 'dtf' update

- Modified `mod_ed_freq_table_server`: added possibility to pass along custom
`dtf::dt_bundle_*()` functions
- Added `filter` arg
- Updated package dependency `dtf` to `0.0.0.9022`

----------

# shimo.eda 0.0.0.9017 (2022-05-31)

Transform function

- Modified `mod_ed_freq_table_server` and `render_grouping_data_table()`: added arg `transform_fn` for custom transform functions

----------

# shimo.eda 0.0.0.9016 (2022-05-31)

Module id

- Modified `mod_ed_freq_table_ui`: new default for `id` and new arg `ns`
- Modified `mod_ed_freq_table_server`: new default for `id`
- Modified `mod_ed_select_ui`: new arg `ns`
- Experimented with nested module namespaces, i.e. passing enclosing `ns` along
to downstream functions. Works conceptionally for `*_ui()` functions (validated
via `shiny_trace_ns_ui()`) but details still not worked out for `*_server()`
functions. See `mod_foo_ui()` and `mod_foo_server()` for details.

----------

# shimo.eda 0.0.0.9015 (2022-05-30)

Tracing shiny NS info

- Modified `mod_ed_freq_table_ui`: uses `shiny_trace_ns_ui()`
- Modified `mod_ed_freq_table_server`: uses `shiny_trace_ns_server()`
- Modified `mod_ed_select_ui`: uses `shiny_trace_ns_ui()`
- Modified `mod_ed_select_server`: uses `shiny_trace_ns_server()`

----------

# shimo.eda 0.0.0.9014 (2022-05-30)

Updated dependency 'dtf' (0.0.0.9017)

- Updated package dependency `dtf` from `0.0.0.9016` to `0.0.0.9017`

----------

# shimo.eda 0.0.0.9013 (2022-05-30)

Updated dependency 'dtf' (0.0.0.9016)

- Updated package dependency `dtf` from `0.0.0.9015` to `0.0.0.9016`

----------

# shimo.eda 0.0.0.9012 (2022-05-30)

Updated dependency 'dtf' (0.0.0.9015)

- Updated package dependency `dtf` from `0.0.0.9014` to `0.0.0.9015`

----------

# shimo.eda 0.0.0.9011 (2022-05-30)

Updated dependency `dtf`

- Updated package dependency `dtf` from `0.0.0.9013` to `0.0.0.9014`

----------

# shimo.eda 0.0.0.9010 (2022-05-30)

Updated dependency `dtf`

- Renamed package dependency from `dti` to `dtf`
- Updated package dependency `dtf` from `0.0.0.9012` to `0.0.0.9013`
- Cleaned up `renv` state

----------

# shimo.eda 0.0.0.9009 (2022-05-30)

Updated dependency `dtf`

- Updated package dependency `dtf` from `0.0.0.9011` to `0.0.0.9012`

----------

# shimo.eda 0.0.0.9008 (2022-05-21)

Dropdown menu

- Added `shinyWidgets::dropdownButton()` in `mod_eda_freq_table`

----------

# shimo.eda 0.0.0.9007 (2022-05-20)

- Changed `tabitem_vertical_space()` to `vertical_space()`

----------

# shimo.eda 0.0.0.9006 (2022-05-17)

Button color and inputs for Y

- Changed button color to `#0c3992`

----------

# shimo.eda 0.0.0.9005 (2022-05-15)

Internationalization

- Added internationalization arguments (routed down to `dtf` functions) to:
    - `mod_eda_select_server()`
    - `mod_eda_freq_table_server()`

----------

# shimo.eda 0.0.0.9004 (2022-05-14)

Outer box

- Added optional boxes for modules `mod_eda_select.R` and `mod_eda_freq_table.R`

----------

# shimo.eda 0.0.0.9002 (2022-05-14)

Bugfix mod_eda_select.R

- Fixed title bug for data table in module `mod_eda_select.R`
- Added server utility `tabitem_vertical_space()`

----------

# shimo.eda 0.0.0.9001 (2022-05-14)

mod_eda_select

- Added module `mod_eda_select.R`
- Refactored module `mod_eda_freq_table.R`

----------

# shimo.eda 0.0.0.9000 (2022-05-14)

Initial commit
