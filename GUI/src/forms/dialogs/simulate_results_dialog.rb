# encoding: utf-8
# Swing Dialog for Simulate Results
class SimulateResultsDialog < STDialogWithOK
  attr_accessor :random_value_label
  attr_accessor :simulate_results_label

  def initialize
    super('Simulate Results')
    self.bounds = Rectangle.new(50, 110, 200, 100)
    Panel.new do |datap|
      datap.layout = BoxLayout.new(datap, BoxLayout::Y_AXIS)
      @random_value_label = make_rv_label(datap)
      make_scroll_panel(datap)
      data_pane.add(datap)
    end
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
