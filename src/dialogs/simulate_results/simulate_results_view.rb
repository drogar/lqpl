

class SimulateResultsView < ApplicationView
  set_java_class com.drogar.qface.dialogs.SimulateResultsDialog

  map :view => "randomValue.text", :model => :random_value_text
  map :view => "simulateResultsLabel.text", :model => :simulate_results_text
end
