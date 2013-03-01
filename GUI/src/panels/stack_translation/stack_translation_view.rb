
class StackTranslationView < ApplicationView
  set_java_class StackTranslationForm #com.drogar.lqpl.screens.StackTranslation
  map   :view => "stack_translation_text", :model => :text
end
