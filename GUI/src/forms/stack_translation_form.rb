  
class StackTranslationForm < Frame
   
  attr_accessor :stack_translation_text
  
  def initialize()
    super('Stack Translation')
    self.layout = BoxLayout.new(:Y_AXIS)
    self.bounds = Rectangle.new(10, 740, 400, 150)
    @stack_translation = Label.new("")
    scrollpane = ScrollPane.new(@stack_translation)
    self.add scrollpane
  end
    
  def stack_translation_text=(new_text)
    @stack_translation.text = new_text
  end
  
  def stack_translation_text
    @stack_translation.text
  end
  
end