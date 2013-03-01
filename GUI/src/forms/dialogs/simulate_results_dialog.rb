java_import javax.swing.JDialog

class SimulateResultsDialog < JDialog

  attr_accessor :random_value_label
  attr_accessor :simulate_results_label
  attr_accessor :ok_button
  
  def initialize()
    super
    self.title = "Simulate Results"
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
    
    Panel.new do |p|
      @ok_button = Button.new("OK") do |okb|
        self.root_pane.default_button = okb
        p.add(okb)
      end
      self.content_pane.add(p)
    end 
    bounds = Rectangle.new(50, 110, 200, 100)
  end
  
  
end