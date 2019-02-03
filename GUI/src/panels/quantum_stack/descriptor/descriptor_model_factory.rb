require 'ensure_json'
require 'classical_descriptor_model'
require 'data_descriptor_model'
require 'qubit_descriptor_model'
require 'value_descriptor_model'
require 'zero_descriptor_model'
# abstract base for the descriptor models
class DescriptorModelFactory
  def self.make_model(in_string)
    json_in = EnsureJSON.new(in_string).as_json
    return ZeroDescriptorModel.new json_in if json_in.key?(:zero)
    return ValueDescriptorModel.new json_in if json_in.key?(:value)
    return ClassicalDescriptorModel.new json_in if json_in.key?(:classical)
    return QubitDescriptorModel.new json_in if json_in.key?(:qubit)
    return DataDescriptorModel.new json_in if json_in.key?(:data)

    raise ModelCreateError, in_string
  end
end
