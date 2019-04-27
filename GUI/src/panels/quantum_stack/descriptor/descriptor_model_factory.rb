require 'ensure_json'
require 'classical_descriptor_model'
require 'data_descriptor_model'
require 'qubit_descriptor_model'
require 'value_descriptor_model'
require 'zero_descriptor_model'
# abstract base for the descriptor models
class DescriptorModelFactory
  DESCRIPTOR_MAP = { zero: ZeroDescriptorModel, value: ValueDescriptorModel, classical: ClassicalDescriptorModel,
                     qubit: QubitDescriptorModel, data: DataDescriptorModel }.freeze

  def self.make_model(in_string)
    json_in = EnsureJSON.new(in_string).as_json

    model_class(json_in).new(json_in)
  rescue StandardError
    raise ModelCreateError, in_string
  end

  def self.model_class(json_with_key)
    DESCRIPTOR_MAP[json_with_key.keys.first]
  end
end
