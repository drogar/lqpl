# encoding: utf-8
require 'quantum_stack_painter'
require 'quantum_stack_form'

#  drawing panel for the tree
class QuantumStackView < ApplicationView
  set_java_class QuantumStackForm
  raw_mapping :make_quantum_stack_painter, nil

  def make_quantum_stack_painter(model, _transfer)
    quantum_stack_panel.quantum_stack_painter =
      QuantumStackPainter.new(model)
  end
end
