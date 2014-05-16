# encoding: utf-8
# zero note model
class ZeroDescriptorModel < AbstractDescriptorModel
  def self.validate_substacks_count(substacks)
    return unless substacks
    fail ModelCreateError, 'Zero stack should not have substacks' if substacks.size > 0
  end

  def initialize(in_string = '{"zero":0}')
    jzero = JSON.parse(in_string, symbolize_names: true)
    if jzero.has_key?(:zero)
      @value = '0'
    else
      fail ModelCreateError, "Zero can not be created with #{in_string}"
    end
  end

  def length
    0
  end
end
