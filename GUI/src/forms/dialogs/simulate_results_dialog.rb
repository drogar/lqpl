# Swing Dialog for Simulate Results
class SimulateResultsDialog < STDialogWithOK
  attr_accessor :random_value_label
  attr_accessor :simulate_results_label

  def initialize
    super('Simulate Results')
    self.bounds = Rectangle.new(50, 110, 200, 100)
    Panel.new { |panelref| initialize_panel(panelref) }
  end

  def initialize_panel(panelref)
    panelref.layout = panel_layout(panelref)
    @random_value_label = make_rv_label(panelref)
    make_scroll_panel(panelref)
    data_pane.add(panelref)
  end

  def panel_layout(panelref)
    BoxLayout.new(panelref, BoxLayout::Y_AXIS)
  end

  def make_rv_label(cpanel)
    Label.new('') { |rvt| cpanel.add(rvt) }
  end

  def make_scroll_panel(cpanel)
    ScrollPane.new do |sp|
      @simulate_results_label = Label.new('') do |lbl|
        sp.viewport_view = lbl
      end
      cpanel.add(sp)
    end
  end
end
