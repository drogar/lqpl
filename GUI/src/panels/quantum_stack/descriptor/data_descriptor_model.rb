class DataDescriptorModel < AbstractDescriptorModel

  def self.validate_substacks_count(substacks)
    raise ModelCreateError, "Data element on stack should have substacks" if !substacks || substacks.size == 0
  end


  def initialize(in_string)
    dpp = DataPatternParser.new(in_string)
    
    @value = dpp.parsed_value
  end


  def length
    @value.length
  end

  def substack_labels
    @value.collect {|v| "#{v[0]}#{v[1] if [] != v[1]}"}
  end
end