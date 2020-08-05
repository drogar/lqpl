require 'simulate_results_dialog'

# view  for the simulate results dialog
class SimulateResultsView < ApplicationView
  set_java_class SimulateResultsDialog

  map view: 'random_value_label.text', model: :random_value_text
  map view: 'simulate_results_label.text', model: :simulate_results_text
  def update(model, transfer)
    super
    @main_view_component.pack
  end
end
