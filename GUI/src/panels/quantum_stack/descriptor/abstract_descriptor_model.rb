require 'ensure_json'
# abstract base for the descriptor models
class AbstractDescriptorModel < ApplicationModel
  attr_accessor :value
  attr_accessor :name

  def initialize
    raise ModelCreateError
  end

  def substack_labels
    nil
  end

  def scalar?
    false
  end
end
