# encoding: utf-8
# value node model
class ValueDescriptorModel < AbstractDescriptorModel
  def self.validate_substacks_count(substacks)
    return unless substacks
    fail ModelCreateError, 'Value element should not have substacks' if substacks.size > 0
  end

  def initialize(in_string)
    value_hash = EnsureJSON.new(in_string).as_json
    @value = value_hash[:value]
    fail ModelCreateError, "Bad VALUE: #{in_string}" unless @value && @value.kind_of?(Numeric)
    @name = nil
  end

  def length
    0
  end
end
