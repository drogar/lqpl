# encoding: utf-8
# abstract base for the descriptor models
class ClassicalDescriptorModel < AbstractDescriptorModel
  def self.validate_substacks_count(substacks)
    fail ModelCreateError,
         'Classical element on stack should have substacks' if !substacks || substacks.size == 0
  end

  def self.valid_classical_kind(v)
    [Numeric, TrueClass, FalseClass].reduce(false) { |a, e| a || v.is_a?(e) }
  end

  def initialize(in_string)
    fail_message = "Invalid Classical: #{in_string}"
    json_c = EnsureJSON.new(in_string).as_json
    @value = json_c[:classical]
    fail ModelCreateError, fail_message unless @value && @value.is_a?(Array)
    fail ModelCreateError, fail_message unless @value.all? { |v| descriptor_ok?(v) }
  end

  def descriptor_ok?(descriptor)
    descriptor && ClassicalDescriptorModel.valid_classical_kind(descriptor)
  end

  def length
    @value.length
  end

  def substack_labels
    @value.map { |v| "#{v}" }
  end
end
