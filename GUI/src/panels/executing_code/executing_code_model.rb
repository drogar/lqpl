require 'code_pointer'
require 'ensure_json'
# model to show the executing code
class ExecutingCodeModel < ApplicationModel
  attr_reader :the_code
  attr_reader :the_code_pointer
  attr_accessor :the_code_was_updated
  alias the_code_was_updated? the_code_was_updated

  def the_code=(in_string)
    ecp = EnsureJSON.new(in_string).as_json
    @the_code = build_code(ecp)
    raise ModelCreateError, 'code xml was ill-formed' unless the_code
  end

  def the_code_pointer=(codepointer_json)
    @the_code_pointer = CodePointer.new codepointer_json
    if code_not_found?
      @the_code_pointer = nil
    else
      @the_code_pointer.normalize(qpo_method_length)
    end
  end

  private

  def build_code(code_as_json)
    Hash[code_as_json.map { |k, v| [k, format_code_method(v)] }]
  end

  def format_code_method(code_line_array)
    code_print_lines = []
    code_line_array.each_with_index { |code_line, i| code_print_lines << format_code_line(i, code_line) }
    code_print_lines
  end

  def format_code_line(line_num, code_line)
    format('%<line>3d  %<code>s', line: line_num, code: code_line)
  end

  def code_not_found?
    !the_code || !qpo_method_exists?
  end

  def qpo_method_exists?
    the_code.key?(the_code_pointer.qpo_method)
  end

  def qpo_method_length
    the_code[the_code_pointer.qpo_method].length
  end
end
