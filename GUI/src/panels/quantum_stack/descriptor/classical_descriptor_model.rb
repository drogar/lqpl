require 'abstract_descriptor_model'

class ClassicalDescriptorModel < AbstractDescriptorModel
  def self.validate_substacks_count(substacks)
    return unless !substacks || substacks.empty?

    raise ModelCreateError,
          'Classical element on stack should have substacks'
  end

  def self.valid_classical_kind(kind)
    [Numeric, TrueClass, FalseClass].reduce(false) { |acc, elem| acc || kind.is_a?(elem) }
  end

  def initialize(in_string)
    fail_message = "Invalid Classical: #{in_string}"
    json_c = EnsureJSON.new(in_string).as_json
    @value = json_c[:classical]
    raise ModelCreateError, fail_message unless @value&.is_a?(Array)
    raise ModelCreateError, fail_message unless @value&.all? { |v| descriptor_ok?(v) }
  end

  def descriptor_ok?(descriptor)
    descriptor && ClassicalDescriptorModel.valid_classical_kind(descriptor)
  end

  def length
    @value.length
  end

  def substack_labels
    @value.map(&:to_s)
  end
end
