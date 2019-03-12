require 'abstract_descriptor_model'

# Data node model
class DataDescriptorModel < AbstractDescriptorModel
  def self.validate_substacks_count(substacks)
    return unless !substacks || substacks.empty?

    raise ModelCreateError,
          'Data element on stack should have substacks'
  end

  def self.data_value_valid(data_value)
    cons_ok(data_value) && address_ok(data_value) && address_elements_ok(data_value)
  end

  def self.cons_ok(data_value)
    data_value && data_value[:cons] && data_value[:cons].is_a?(String)
  end

  def self.address_ok(data_value)
    data_value && data_value[:addresses].is_a?(Array)
  end

  def self.address_elements_ok(data_value)
    data_value && data_value[:addresses].each do |a|
      return false unless a&.is_a?(Integer)
    end
  end

  def initialize(in_string)
    fail_message = "Invalid Algebraic data: #{in_string}"
    json_d = EnsureJSON.new(in_string).as_json
    @value = json_d[:data]
    raise ModelCreateError, fail_message unless @value&.is_a?(Array)

    @value.each do |data_value|
      raise ModelCreateError, fail_message unless DataDescriptorModel.data_value_valid(data_value)
    end
  end

  def length
    @value.length
  end

  def substack_labels
    @value.map { |v| "#{v[:cons]}#{v[:addresses] if v[:addresses] != []}" }
  end
end
