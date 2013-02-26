class DumpForm < ScrollableLabel
   
  attr_accessor :dump_text
  
  def initialize()
    super("Dump",Rectangle.new(430, 660, 600, 200))
  end
    
  def dump_text=(new_text)
    self.the_scrolling_label.text = new_text
  end
  
  def dump_text
    self.the_scrolling_label.text
  end
  
end