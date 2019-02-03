# classical stack model
# Currently just an html dump
class ClassicalStackModel < ApplicationModel
  attr_reader :classical_stack

  def classical_stack_text=(_unused); end

  def classical_stack_text
    cs = @classical_stack || []
    return '' if cs == []

    inside = cs.map(&:to_s).join('<br />')
    '<html>' + inside + '</html>'
  end

  def to_a
    @classical_stack
  end

  def classical_stack=(json_input)
    @classical_stack = JSON.parse(json_input, symbolize_names: true)[:cstack]
  end
end
