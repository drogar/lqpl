# encoding: utf-8
# classical stack model
# Currently just an html dump
class ClassicalStackModel < ApplicationModel
  attr_accessor :classical_stack_text
  attr_accessor :classical_stack

  def classical_stack_text=(_)
  end

  def classical_stack_text
    cs = @classical_stack || []
    return '' if cs == []
    inside = cs.map { |c| "#{c}" }.join('<br />')
    '<html>' + inside + '</html>'
  end

  def to_a
    @classical_stack
  end

  def classical_stack=(xml_input)
    cpp = ClassicalPatternParser.new xml_input
    @classical_stack = cpp.parsed_value
  end
end
