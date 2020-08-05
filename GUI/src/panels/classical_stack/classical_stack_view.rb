require 'classical_stack_form'

# classical stack
class ClassicalStackView < ApplicationView
  set_java_class ClassicalStackForm

  map view: 'classical_stack_text', model: :classical_stack_text
end
