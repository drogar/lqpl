require 'abstract_descriptor_model'

# qubit node model
class QubitDescriptorModel < AbstractDescriptorModel
  def self.validate_substacks_count(substacks)
    return unless !substacks || substacks.empty? || substacks.size > 4

    raise ModelCreateError,
          "Qubit on stack must have 1 - 4 substacks, not #{!substacks ? 'be nil' : substacks.size}"
  end

  VALID_PAIRS = { 'ZZ' => [0, 0], 'OO' => [1, 1], 'ZO' => [0, 1], 'OZ' => [1, 0] }.freeze

  def initialize(in_string)
    fail_message = "Invalid Qubit: #{in_string}"

    json_q = EnsureJSON.new(in_string).as_json
    raise ModelCreateError, fail_message unless json_q[:qubit]

    @value = json_q[:qubit].map { |q| VALID_PAIRS[q] }
    raise ModelCreateError, fail_message if @value.empty? || @value.length > 4
    raise ModelCreateError, fail_message if @value.include?(nil)
  end

  def length
    @value.length
  end

  def substack_labels
    @value.map { |v| "#{v[0]}#{v[1]}" }
  end
end
