# encoding: utf-8
# qubit node model
class QubitDescriptorModel < AbstractDescriptorModel
  def self.validate_substacks_count(substacks)
    if !substacks || substacks.size == 0 || substacks.size > 4
      fail ModelCreateError,
           "Qubit on stack must have 1 - 4 substacks, not #{!substacks ? 'be nil' : substacks.size}"
    end
  end

  VALID_PAIRS = { 'ZZ' => [0, 0], 'OO' => [1, 1], 'ZO' => [0, 1], 'OZ' => [1, 0] }

  def initialize(in_string)
    fail_message = "Invalid Qubit: #{in_string}"

    json_q = JSON.parse(in_string, symbolize_names: true)
    fail ModelCreateError, fail_message unless json_q[:qubit]
    @value = json_q[:qubit].map { |q| VALID_PAIRS[q] }
    fail ModelCreateError, fail_message if @value.length == 0 || @value.length > 4
    fail ModelCreateError, fail_message if @value.include?(nil)
  end

  def length
    @value.length
  end

  def substack_labels
    @value.map { |v| "#{v[0]}#{v[1]}" }
  end
end
