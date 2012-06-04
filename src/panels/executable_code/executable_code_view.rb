java_import javax.swing.JTextArea

class ExecutableCodeView < ApplicationView
  set_java_class com.drogar.qface.screens.ExecutingCode

  raw_mapping :set_up_tabbed_views, nil

  attr :qpo_method_to_tab_map
  attr :qpo_method_and_line_to_selection_start_and_end_map

  def set_up_tabbed_views(model,transfer)
    if model.the_code_was_updated?
      create_tabbed_views(model.the_code)
    end
    set_highlight_for_code_pointer(model.the_code_pointer)
  end

  def create_tabbed_views(code_map)
    code_tab_pane = codeTabbedPane
    codeTabbedPane.remove_all
    @qpo_method_to_tab_map = {}
    @qpo_method_and_line_to_selection_start_and_end_map = {}
    i = 0
    code_map.each do |qpo_method, qpo_ins|
      instructions_text_area = JTextArea.new(qpo_ins.join("\n"), qpo_ins.length, 80)
      instructions_text_area.editable = false
      code_tab_pane.add_tab(qpo_method.to_s, instructions_text_area)
      #instructions_text_area.visible  = true
      @qpo_method_to_tab_map[qpo_method] = i
      text_len=0
      qpo_ins.each_with_index do |ins_line, ind|
        @qpo_method_and_line_to_selection_start_and_end_map["#{qpo_method}--#{ind}"] = [text_len, text_len+1+ins_line.length]
        text_len += 1+ins_line.length
      end
      i += 1
    end
    #code_tab_pane.visible = true
    #code_tab_pane.show
  end

  def set_highlight_for_code_pointer(code_pointer_map)
    puts code_pointer_map
    return if !@qpo_method_to_tab_map[code_pointer_map.keys[0]]
    codeTabbedPane.selected_index = @qpo_method_to_tab_map[code_pointer_map.keys[0]]
    return if !@qpo_method_and_line_to_selection_start_and_end_map["#{code_pointer_map.keys[0]}--#{code_pointer_map.values[0]}"]
    selection_bounds = @qpo_method_and_line_to_selection_start_and_end_map["#{code_pointer_map.keys[0]}--#{code_pointer_map.values[0]}"]
    puts selection_bounds
    jt = codeTabbedPane.selected_component
    jt.request_focus_in_window
    jt.selection_end = selection_bounds[1]
    jt.selection_start = selection_bounds[0]
  end
end
