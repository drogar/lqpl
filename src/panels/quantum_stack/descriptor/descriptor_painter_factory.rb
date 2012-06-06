
require "panels/quantum_stack/descriptor/abstract_descriptor_painter"
require "panels/quantum_stack/descriptor/zero_descriptor_painter"
require "panels/quantum_stack/descriptor/value_descriptor_painter"
require "panels/quantum_stack/descriptor/classical_descriptor_painter"
require "panels/quantum_stack/descriptor/qubit_descriptor_painter"
require "panels/quantum_stack/descriptor/data_descriptor_painter"

class DescriptorPainterFactory
  def self.make_painter(model)
    case model
    when ClassicalDescriptorModel  then ClassicalDescriptorPainter.new(model)
    when DataDescriptorModel       then DataDescriptorPainter.new(model)
    when QubitDescriptorModel      then QubitDescriptorPainter.new(model)
    when ValueDescriptorModel      then ValueDescriptorPainter.new(model)
    when ZeroDescriptorModel       then ZeroDescriptorPainter.new(model)
    end
  end
end