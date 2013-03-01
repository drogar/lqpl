
class QuantumStackView < ApplicationView
  set_java_class QuantumStackForm
  raw_mapping  :make_quantum_stack_painter,nil
    
  def make_quantum_stack_painter(model,transfer)
    quantum_stack_panel.quantum_stack_painter =  
      QuantumStackPainter.new(model)
  end
end
