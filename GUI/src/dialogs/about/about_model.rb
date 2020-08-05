require 'lqpl_version'
# Model for the about dialog
class AboutModel
  attr_accessor :about_text

  def initialize
    @about_text = LqplVersion::ABOUT_STRING
  end
end
