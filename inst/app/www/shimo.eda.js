function shimo_eda_mod_freq_table_js(ns_prefix) {

  $("#" + ns_prefix + "group_by_ui").on("click", ".shiny-bound-input", function() {
    Shiny.setInputValue(ns_prefix + "ui_to_delete_id", this.id, { priority: "event"});
    Shiny.setInputValue(ns_prefix + "ui_to_delete_id", "", { priority: "event"});
  });

}
