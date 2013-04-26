java_import javax.swing.JDialog

class SimulateResultsDialog < JDialog

  attr_accessor :random_value_label
  attr_accessor :simulate_results_label
  attr_accessor :ok_button

  def initialize()
    super
    self.title = "Simulate Results"
    self.content_pane = make_content_panel
    self.bounds = Rectangle.new(50, 110, 200, 100)
  end

  def make_content_panel
    Panel.new do |cp|
      cp.layout = BoxLayout.new(cp,BoxLayout::Y_AXIS)
      @random_value_label = make_rv_label(cp)
      make_scroll_panel(cp)
      make_button_panel(cp)
    end
  end
  def make_rv_label(cpanel)
    Label.new("") do |rvt|
      cpanel.add(rvt)
    end
  end
  def make_scroll_panel(cpanel)
    ScrollPane.new do |sp|
      @simulate_results_label = Label.new("") do |lbl|
        sp.viewport_view=lbl
      end
      cpanel.add(sp)
    end
  end
  def make_button_panel(cpanel)
    Panel.new do |p|
      @ok_button = Button.new("OK") do |okb|
        self.root_pane.default_button = okb
        p.add(okb)
      end
      cpanel.add(p)
    end
  end


end