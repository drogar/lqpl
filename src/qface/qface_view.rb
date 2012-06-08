class QfaceView < ApplicationView
  set_java_class com.drogar.qface.screens.QuantumEmulatorMainScreen
  map :view => "controlPanel.visible", :model => :control_panel_visible
  map :view => "stepSpinner.model.value", :model => :step_spinner
  map :view => "stepButton.enabled", :model => :step_enabled
  map :view => "goButton.enabled", :model => :go_enabled

  map :view => "viewClassicalStackMI.enabled", :model => :view_menu_classical_stack_enabled
  map :view => "viewDumpMI.enabled", :model => :view_menu_dump_enabled
  map :view => "viewExecutingCodeMI.enabled", :model => :view_menu_executing_code_enabled
  map :view => "viewStackTranslationMI.enabled", :model => :view_menu_stack_translation_enabled

  map :view => "viewClassicalStackMI.text", :model => :view_menu_classical_stack_text
  map :view => "viewDumpMI.text", :model => :view_menu_dump_text
  map :view => "viewExecutingCodeMI.text", :model => :view_menu_executing_code_text
  map :view => "viewStackTranslationMI.text", :model => :view_menu_stack_translation_text


end
