
class StackTranslationView < ApplicationView
  set_java_class com.drogar.lqpl.screens.StackTranslation
  map   :view => "stackTranslation.text", :model => :text
end
