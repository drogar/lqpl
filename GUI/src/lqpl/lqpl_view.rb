require 'lqpl_menu'

class LqplView < ApplicationView
  set_java_class com.drogar.lqpl.screens.QuantumEmulatorMainScreen

  attr_accessor :the_menu
  map :view => "spinnerPanel.visible", :model => :spinner_panel_visible
  map :view => "buttonPanel.visible", :model => :button_panel_visible

  map :view => "stepSpinner.model.value", :model => :step_spinner
  map :view => "recursionSpinner.model.value", :model => :recursion_spinner
  map :view => "treeDepthSpinner.model.value", :model => :tree_depth_spinner

  map :view => "messagesTextArea.text", :model => :messages_text

  map :view => "stepButton.enabled", :model => :step_enabled
  map :view => "goButton.enabled", :model => :go_enabled

  map :view => "the_menu.view_classical_stack.enabled", :model => :view_menu_classical_stack_enabled
  map :view => "the_menu.view_dump.enabled", :model => :view_menu_dump_enabled
  map :view => "the_menu.view_executing_code.enabled", :model => :view_menu_executing_code_enabled
  map :view => "the_menu.view_stack_translation.enabled", :model => :view_menu_stack_translation_enabled

  map :view => "the_menu.view_classical_stack.text", :model => :view_menu_classical_stack_text
  map :view => "the_menu.view_dump.text", :model => :view_menu_dump_text
  map :view => "the_menu.view_executing_code.text", :model => :view_menu_executing_code_text
  map :view => "the_menu.view_stack_translation.text", :model => :view_menu_stack_translation_text

  #map :view => "frameTitle", :model => :frame_title
  raw_mapping :set_title,nil


  def load(*args)
    @the_menu=LqplMenu.new(self)
  end

  def set_menu_bar(mbar)
    @main_view_component.setJMenuBar(mbar)
  end

  def set_title(model,trans)
    @main_view_component.title = model.frame_title
  end

  def the_frame
    @main_view_component
  end
end
