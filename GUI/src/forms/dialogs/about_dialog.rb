
class AboutDialog < STDialogWithOK

  attr_accessor :about_data_label
  
  def initialize()
    super("About LQPL")
    current_components = self.content_pane.components
    self.content_pane = Panel.new do |cp|
      cp.layout = BoxLayout.new(cp,BoxLayout::Y_AXIS)
    end
    
    
    Panel.new do |p|
      @about_data_label = Label.new("")
      p.add(@about_data_label)
      self.content_pane.add(p)
    end
    current_components.each {|c| self.content_pane.add(c)}
  end
  
  
end