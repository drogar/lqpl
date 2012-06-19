

class SimulateResultsView < ApplicationView
  set_java_class com.drogar.lqpl.dialogs.SimulateResultsDialog

  map :view => "randomValue.text", :model => :random_value_text
  map :view => "simulateResultsLabel.text", :model => :simulate_results_text
  def update(model,transfer)
    super
    @main_view_component.pack
  end
end
