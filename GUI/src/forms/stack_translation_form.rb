  
class StackTranslationForm < STFrame
   
  attr_accessor :stack_translation_text
  
  def initialize()
    super()
    self.title = "Stack Translation"
    self.bounds = Rectangle.new(10, 740, 400, 150)
    @stack_translation = Label.new("")
    scrollpane = ScrollPane.new(@stack_translation)
    self.content_pane = scrollpane
  end
    
  def stack_translation_text=(new_text)
    @stack_translation.text = new_text
  end
  
  def stack_translation_text
    @stack_translation.text
  end
  
end