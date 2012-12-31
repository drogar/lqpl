class ClassicalDescriptorModel< AbstractDescriptorModel
  
  def self.validate_substacks_count(substacks)
    raise ModelCreateError, "Classical element on stack should have substacks" if !substacks || substacks.size == 0
  end

  def initialize(in_string)
    cpp = ClassicalPatternParser.new in_string
    @value = cpp.parsed_value
  end

  def length
    @value.length
  end

  def substack_labels
    @value.collect {|v| "#{v}"}
  end

end