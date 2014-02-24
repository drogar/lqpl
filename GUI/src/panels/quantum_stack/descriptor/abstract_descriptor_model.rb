# encoding: utf-8
# abstract base for the descriptor models
class AbstractDescriptorModel < ApplicationModel
  attr_accessor :value
  attr_accessor :name

  def self.make_instance(in_string)
    case in_string
    when /^<Zero/ then ZeroDescriptorModel.new in_string
    when /^<Valu/ then ValueDescriptorModel.new in_string
    when /^<Clas/ then ClassicalDescriptorModel.new in_string
    when /^<Qubi/ then QubitDescriptorModel.new in_string
    when /^<Alge/ then DataDescriptorModel.new in_string
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
