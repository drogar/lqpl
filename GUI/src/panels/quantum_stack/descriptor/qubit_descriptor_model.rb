require 'abstract_descriptor_model'

# qubit node model
class QubitDescriptorModel < AbstractDescriptorModel
  def self.validate_substacks_count(substacks)
    return unless substacks_invalid(substacks)

    raise ModelCreateError,
          "Qubit on stack must have 1 - 4 substacks, not #{!substacks ? 'be nil' : substacks.size}"
  end

  def self.substacks_invalid(substacks)
    !substacks || substacks.empty? || substacks.size > 4
  end

  VALID_PAIRS = { 'ZZ' => [0, 0], 'OO' => [1, 1], 'ZO' => [0, 1], 'OZ' => [1, 0] }.freeze

  def initialize(in_string)
    fm = "Invalid Qubit: #{in_string}"
    json_q = EnsureJSON.new(in_string).as_json
    raise ModelCreateError, fm unless json_q[:qubit]

    make_value(json_q, fm)
  end

  def make_value(json_q, failm)
    @value = json_q[:qubit].map { |q| VALID_PAIRS[q] }
    raise ModelCreateError, failm if invalid?
  end

  def invalid?
    @value.empty? || @value.length > 4 || @value.include?(nil)
  end

  def length
    @value.length
  end

  def substack_labels
    @value.map { |v| "#{v[0]}#{v[1]}" }
  end
end
