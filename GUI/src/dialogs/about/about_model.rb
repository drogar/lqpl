require 'version'
class AboutModel
  attr_accessor :about_text

  def initialize
    @about_text = ABOUT_STRING
  end
end
