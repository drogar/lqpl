class LqplView < ApplicationView
  set_java_class com.drogar.lqpl.screens.QuantumEmulatorMainScreen
  map :view => "spinnerPanel.visible", :model => :spinner_panel_visible
  map :view => "buttonPanel.visible", :model => :button_panel_visible

  map :view => "stepSpinner.model.value", :model => :step_spinner
  map :view => "recursionSpinner.model.value", :model => :recursion_spinner
  map :view => "treeDepthSpinner.model.value", :model => :tree_depth_spinner

  map :view => "messagesTextArea.text", :model => :messages_text

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

  #map :view => "frameTitle", :model => :frame_title
  raw_mapping :set_title,nil

  def set_title(model,trans)
    @main_view_component.title = model.frame_title
  end

end
