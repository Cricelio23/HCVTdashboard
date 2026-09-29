General instructions
- Default behavior is analysis-only.
- No file edits without explicit user approval in the same turn.
- Only modify user-listed files. If another file is required, stop and request approval first.
- Any new request or scope change requires new approval.
 
Module acceptance checklist
- UI structure exists in mod_<name>_ui(id), not only as placeholder.
- Server logic exists in mod_<name>_server(...), including required outputs.
- All module input/output ids are namespaced.
- conditionalPanel conditions reference existing inputs/outputs.
- tabBox/tabset containers contain only valid tab panels/menus.
- Text shown to users is translated and reactive to lang().
- New/changed files parse successfully.
- Add package namespaces like shiny::selectInput(), dplyr::filter(), plotly::ggplotly(), gt::gt_output()