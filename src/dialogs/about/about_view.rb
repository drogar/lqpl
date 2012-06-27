
class AboutView < ApplicationView
  set_java_class com.drogar.lqpl.about.AboutDialog


  map :view => "aboutDataLabel.text", :model => :about_text

  def on_first_update(model,transfer)
    super(model,transfer)
    @main_view_component.pack
  end
end
