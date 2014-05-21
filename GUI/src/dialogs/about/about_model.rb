# encoding: utf-8
require 'version'
# Model for the about dialog
class AboutModel
  attr_accessor :about_text

  def initialize
    @about_text = ABOUT_STRING
  end
end
