# encoding: utf-8
require 'classical_descriptor_painter'
require 'data_descriptor_painter'
require 'qubit_descriptor_painter'
require 'value_descriptor_painter'
require 'zero_descriptor_painter'
# factory to create the data descriptor's painter
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
