class QubitDescriptorModel < AbstractDescriptorModel

  def self.validate_substacks_count(substacks)
    raise ModelCreateError, "Qubit on stack should have 1 - 4 substacks, not #{!substacks ? "be nil" : substacks.size}" if !substacks || substacks.size == 0 or substacks.size > 4
  end
 
  def initialize(in_string)
    qpp = QubitPatternParser.new in_string
    @value = qpp.parsed_value
  end

  def length
    @value.length
  end

  def substack_labels
    @value.collect {|v| "#{v[0]}#{v[1]}"}
  end
end