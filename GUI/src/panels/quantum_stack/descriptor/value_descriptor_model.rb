

class ValueDescriptorModel < AbstractDescriptorModel

  def self.validate_substacks_count(substacks)
    return unless substacks
    raise ModelCreateError, "Value element should not have substacks" if substacks.size > 0
  end
  
  def initialize(in_string)
    @value = @value = (ValuePatternParser.new in_string).parsed_value
    @name = nil
  end

  def length
    0
  end

end