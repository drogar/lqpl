# encoding: utf-8
# abstract base for the descriptor models
class AbstractDescriptorModel < ApplicationModel
  attr_accessor :value
  attr_accessor :name

  def self.make_instance(in_string)
    json_in = EnsureJSON.new(in_string).as_json
    return ZeroDescriptorModel.new json_in if json_in.key?(:zero)
    return ValueDescriptorModel.new json_in if json_in.key?(:value)
    return ClassicalDescriptorModel.new json_in if json_in.key?(:classical)
    return QubitDescriptorModel.new json_in if json_in.key?(:qubit)
    return DataDescriptorModel.new json_in if json_in.key?(:data)
    fail ModelCreateError, in_string
  end

  def initialize
    fail ModelCreateError
  end

  def substack_labels
    nil
  end
end
