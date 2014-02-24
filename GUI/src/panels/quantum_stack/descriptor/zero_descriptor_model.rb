# encoding: utf-8
# zero note model
class ZeroDescriptorModel < AbstractDescriptorModel
  def self.validate_substacks_count(substacks)
    return unless substacks
    fail ModelCreateError, 'Zero stack should not have substacks' if substacks.size > 0
  end

  def initialize(in_string = '<Zero/>')
    @value = (ZeroPatternParser.new in_string).parsed_value
  end

  def length
    0
  end
end
