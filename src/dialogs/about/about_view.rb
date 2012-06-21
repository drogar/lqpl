
class AboutView < ApplicationView
  set_java_class com.drogar.lqpl.about.AboutDialog


  map :view => "aboutDataLabel.text", :model => :about_text
end
