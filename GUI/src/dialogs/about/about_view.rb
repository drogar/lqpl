# encoding: utf-8
require 'about_dialog'
# view  for the about dialog
class AboutView < ApplicationView
  set_java_class AboutDialog

  map view: 'about_data_label.text', model: :about_text

  def on_first_update(model, transfer)
    super(model, transfer)
    @main_view_component.pack
  end
end
