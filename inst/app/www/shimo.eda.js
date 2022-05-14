function shimo_eda_mod_freq_table_js(ns_prefix) {
  $("#" + ns_prefix + "grouping_ui").on("click", ".delete_btn.shiny-bound-input", function() {
    Shiny.setInputValue(ns_prefix + "ui_to_delete_id", this.id, { priority: "event"});
    Shiny.setInputValue(ns_prefix + "ui_to_delete_id", "", { priority: "event"});
  });
}

function shimo_eda_mod_select_js(ns_prefix) {
  $("#" + ns_prefix + "select_ui").on("click", ".delete_btn.shiny-bound-input", function() {
    Shiny.setInputValue(ns_prefix + "ui_to_delete_id", this.id, { priority: "event"});
    Shiny.setInputValue(ns_prefix + "ui_to_delete_id", "", { priority: "event"});
  });
}
