# encoding: utf-8
# qubit node model
class QubitDescriptorModel < AbstractDescriptorModel
  def self.validate_substacks_count(substacks)
    if !substacks || substacks.size == 0 || substacks.size > 4
      fail ModelCreateError,
           "Qubit on stack must have 1 - 4 substacks, not #{!substacks ? 'be nil' : substacks.size}"
    end
  end

  def initialize(in_string)
    qpp = QubitPatternParser.new in_string
    @value = qpp.parsed_value
  end

  def length
    @value.length
  end

  def substack_labels
    @value.map { |v| "#{v[0]}#{v[1]}" }
  end
end
