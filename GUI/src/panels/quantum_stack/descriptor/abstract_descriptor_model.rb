# encoding: utf-8
# abstract base for the descriptor models
class AbstractDescriptorModel < ApplicationModel
  attr_accessor :value
  attr_accessor :name

  def self.make_instance(in_string)
    case in_string
    when /^{"zero/ then ZeroDescriptorModel.new in_string
    when /^{"valu/ then ValueDescriptorModel.new in_string
    when /^{"clas/ then ClassicalDescriptorModel.new in_string
    when /^{"qubi/ then QubitDescriptorModel.new in_string
    when /^{"data/ then DataDescriptorModel.new in_string
    else fail ModelCreateError, in_string
    end
  end

  def initialize
    fail ModelCreateError
  end

  def substack_labels
    nil
  end
end
