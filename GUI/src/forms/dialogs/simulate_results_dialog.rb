class SimulateResultsDialog < STDialogWithOK

  attr_accessor :random_value_label
  attr_accessor :simulate_results_label
  
  def initialize()
    super("Simulate Results")
    current_components = self.content_pane.components
    self.content_pane = Panel.new do |cp|
      cp.layout = BoxLayout.new(cp,BoxLayout::Y_AXIS)
    end
    
    @random_value_label = Label.new("") do |rvt|
      self.content_pane.add(rvt)
    end
    
    ScrollPane.new do |sp|
      @simulate_results_label = Label.new("") do |lbl|
        sp.viewport_view=lbl
      end
      self.content_pane.add(sp)
    end
    
    
    current_components.each {|c| self.content_pane.add(c)}
    bounds = Rectangle.new(50, 110, 200, 100)
  end
  
  
end