# encoding: utf-8
# Class to control the simulate results dialog
class SimulateResultsController < ApplicationController
  set_model 'SimulateResultsModel'
  set_view 'SimulateResultsView'
  set_close_action :dispose

  def set_simulate_data(xml_data, stack_translation)
    model.stack_translation = stack_translation
    model.simulate_results = xml_data
    update_view
  end

  def simulate_data
    model.simulate_results_text
  end

  def set_simulate_results(recursion_depth, stack_translation)
    set_simulate_data(lqpl_emulator_server_connection.do_simulate(recursion_depth),
                      stack_translation)
  end

  def ok_button_action_performed
    dispose
  end
end
