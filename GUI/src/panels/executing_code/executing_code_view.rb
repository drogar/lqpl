require 'executing_code_form'

# View for the executing code - a tabbed vew pane
class ExecutingCodeView < ApplicationView
  set_java_class ExecutingCodeForm
  WIDTH_OF_TEXT_PANE = 60

  raw_mapping :set_up_tabbed_views, nil

  attr_accessor :qpo_method_to_tab_map
  attr_accessor :qpo_method_and_line_to_selection_start_and_end_map

  def set_up_tabbed_views(model, _transfer)
    create_tabbed_views(model.the_code) if model.the_code_was_updated?
    highlight_the_code_pointer(model.the_code_pointer)
  end

  def create_tabbed_views(code_map)
    code_tab_pane = reset_tabbed_panes_and_maps
    cp = CodePointer.new ''
    code_map.each_with_index do |qpo_method_and_ins, i|
      create_a_tabbed_view(qpo_method_and_ins, code_tab_pane, cp, i)
    end
  end

  def create_a_tabbed_view(qpo_method_and_ins, code_tab_pane, code_pointer, index)
    qpo_method = qpo_method_and_ins[0]
    qpo_ins = qpo_method_and_ins[1]
    code_tab_pane.add_tab(qpo_method.to_s, ExecutingCodeView.init_scroll_pane(qpo_ins))
    @qpo_method_to_tab_map[qpo_method] = index
    code_pointer.qpo_method = qpo_method
    process_instructions_text(code_pointer, qpo_ins)
  end

  def process_instructions_text(code_pointer, qpo_ins)
    text_len = 0
    qpo_ins.each_with_index do |ins_line, ind|
      code_pointer.line_number = ind
      add_to_selection_start_and_end_map(code_pointer, ins_line, text_len)
      text_len += 1 + ins_line.length
    end
  end

  def reset_tabbed_panes_and_maps
    code_tabbed_pane.remove_all
    @qpo_method_to_tab_map = {}
    @qpo_method_and_line_to_selection_start_and_end_map = {}
    code_tabbed_pane
  end

  def add_to_selection_start_and_end_map(code_pointer, ins_line, text_len)
    @qpo_method_and_line_to_selection_start_and_end_map[code_pointer.mangle_to_selection_key] =
      [text_len, text_len + 1 + ins_line.length]
  end

  def self.init_scroll_pane(qpo_ins)
    JScrollPane.new(ExecutingCodeView.init_instructions_text_area(qpo_ins))
  end

  def self.init_instructions_text_area(qpo_ins)
    instructions_text_area = JTextArea.new(qpo_ins.join("\n"), qpo_ins.length, WIDTH_OF_TEXT_PANE)
    instructions_text_area.editable = false
    instructions_text_area.selection_start = 0
    instructions_text_area.selection_end = 0
    instructions_text_area
  end

  # TODO: - revise this and code_pointer to know when just the line changes,
  #        rather than the whole thing.

  def highlight_the_code_pointer(code_pointer)
    return unless @qpo_method_to_tab_map[code_pointer.qpo_method]

    code_tabbed_pane.selected_index = @qpo_method_to_tab_map[code_pointer.qpo_method]
    selection_key = code_pointer.mangle_to_selection_key
    highlight_selection_in_view(@qpo_method_and_line_to_selection_start_and_end_map[selection_key])
  end

  def highlight_selection_in_view(selection_bounds)
    jt = code_tab_selection_view
    jt.request_focus(true)
    # request_focus is a deprecated method, but otherwise the highlight
    # does not show when switching qpo_methods
    reset_selection(jt)
    set_selection_to_bounds(jt, selection_bounds[0], selection_bounds[1]) if selection_bounds
  end

  private

  def code_tab_selection_view
    code_tabbed_pane.selected_component.viewport.view
  end

  def reset_selection(current_selection)
    current_selection.selection_start = 0 # reset to handle "use" case where we go back (loop) in the code
    current_selection.selection_end = 0
  end

  def set_selection_to_bounds(current_selection, start, finish)
    current_selection.selection_start = start
    current_selection.selection_end = finish
  end
end
