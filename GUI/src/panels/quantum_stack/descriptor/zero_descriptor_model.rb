require 'abstract_descriptor_model'

# zero note model
class ZeroDescriptorModel < AbstractDescriptorModel
  def self.validate_substacks_count(substacks)
    return unless substacks
    raise ModelCreateError, 'Zero stack should not have substacks' unless substacks.empty?
  end

  def initialize(in_string = '{"zero":0}')
    jzero = EnsureJSON.new(in_string).as_json
    if jzero.key?(:zero)
      @value = '0'
      return
    end

    raise ModelCreateError, "Zero can not be created with #{in_string}"
  end

  def length
    0
  end
end
