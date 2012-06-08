
class ClassicalStackView < ApplicationView
  set_java_class com.drogar.qface.screens.ClassicalStack

  map  :view => "classicalStack.text", :model => :classical_stack_text
end
