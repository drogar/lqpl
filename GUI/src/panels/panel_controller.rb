# encoding: utf-8
# abstract base class for the panel controllers, plus initial creation of all the
# controllers and add them into a class variable.
class PanelController < ApplicationController
  @controllers = {}
  class << self; attr_accessor :controllers end
  def self.inherited(subclass)
    controllers[class_name_to_sym(subclass)] = subclass
  end

  def self.class_name_to_sym(klass)
    c = klass.name
    c.match(/([[:upper:]][[:lower:]]*).*/)[1].to_sym
  end

  def self.controller_from_name(name_array)
    name_array.map(&:to_sym).reduce(nil) { |a, e| a || controllers[e] }
  end

  def update_on_lqpl_model_trim
    false
  end
end
