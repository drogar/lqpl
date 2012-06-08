
class StackTranslationView < ApplicationView
  set_java_class com.drogar.qface.screens.StackTranslation
  map   :view => "stackTranslation.text", :model => :text
end
