WIDTH_OF_TEXT_PANE = 60

class ExecutableCodeView < ApplicationView
  set_java_class com.drogar.lqpl.screens.ExecutingCode

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
    code_tab_pane = reset_tabbed_panes_and_maps
    i = 0
    code_map.each do |qpo_method, qpo_ins|
      code_tab_pane.add_tab(qpo_method.to_s, ExecutableCodeView::init_scroll_pane(qpo_ins))
      @qpo_method_to_tab_map[qpo_method] = i
      text_len=0
      qpo_ins.each_with_index do |ins_line, ind|
        add_to_selection_start_and_end_map(qpo_method, ins_line, ind, text_len)
        text_len += 1+ins_line.length
      end
      i += 1
    end
  end
  
  def reset_tabbed_panes_and_maps
    codeTabbedPane.remove_all
    @qpo_method_to_tab_map = {}
    @qpo_method_and_line_to_selection_start_and_end_map = {}
    codeTabbedPane
  end
  
  def add_to_selection_start_and_end_map(qpo_method,ins_line,index,text_len)
    @qpo_method_and_line_to_selection_start_and_end_map[ExecutableCodeView::make_selection_key(qpo_method,index)] = [text_len, text_len+1+ins_line.length]    
  end
  
  def self.init_scroll_pane qpo_ins
    JScrollPane.new(ExecutableCodeView::init_instructions_text_area qpo_ins)
  end
  
  def self.init_instructions_text_area qpo_ins
    instructions_text_area = JTextArea.new(qpo_ins.join("\n"), qpo_ins.length, WIDTH_OF_TEXT_PANE)
    instructions_text_area.editable = false
    instructions_text_area.selection_start = 0
    instructions_text_area.selection_end = 0
    instructions_text_area
  end
  
  #todo - revise this and code_pointer to know when just the line changes, rather than the whole thing.
  def set_highlight_for_code_pointer(code_pointer)
    return if !@qpo_method_to_tab_map[code_pointer.qpo_method]
    codeTabbedPane.selected_index = @qpo_method_to_tab_map[code_pointer.qpo_method]
    selection_key = ExecutableCodeView::mangle_code_pointer_to_selection_key(code_pointer)
    return if !@qpo_method_and_line_to_selection_start_and_end_map[selection_key]
    selection_bounds = @qpo_method_and_line_to_selection_start_and_end_map[selection_key]
    set_selection_bounds_in_view(codeTabbedPane, selection_bounds)
  end
  
  def set_selection_bounds_in_view(codeTabbedPane,selection_bounds)
    jt = codeTabbedPane.selected_component.viewport.view
    jt.request_focus(true) # deprecated method, but otherwise the highlight does not show when switching qpo_methods
    jt.selection_start = 0  # reset to handle "use" case where we go back (loop) in the code
    jt.selection_end = selection_bounds[1]
    jt.selection_start = selection_bounds[0]
  end

  def self.mangle_code_pointer_to_selection_key(cp)
    self.make_selection_key(cp.qpo_method, cp.line_number)
  end

  def self.make_selection_key(m,l)
    "#{m}--#{l}"
  end
end
