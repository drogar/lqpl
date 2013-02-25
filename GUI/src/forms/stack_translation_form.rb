  
class StackTranslationForm < ScrollableLabel
   
  attr_accessor :stack_translation_text
  
  def initialize()
    super("Stack Translation",Rectangle.new(10, 740, 400, 150))
  end
    
  def stack_translation_text=(new_text)
    self.the_scrolling_label.text = new_text
  end
  
  def stack_translation_text
    self.the_scrolling_label.text
  end
  
end