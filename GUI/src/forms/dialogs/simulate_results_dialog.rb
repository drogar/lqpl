class SimulateResultsDialog < STDialogWithOK

  attr_accessor :random_value_label
  attr_accessor :simulate_results_label

  def initialize()
    super("Simulate Results")
    self.bounds = Rectangle.new(50, 110, 200, 100)
    @random_value_label = make_rv_label(content_pane)
    make_scroll_panel(content_pane)
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
end