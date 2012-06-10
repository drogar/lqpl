class SimulateResultsController < ApplicationController
  set_model 'SimulateResultsModel'
  set_view 'SimulateResultsView'
  set_close_action :dispose

  attr_accessor :server_connection

  def server_connection=(sc)
    @server_connection = sc
    @server_connection.connect if !@server_connection.connected?
  end

  def set_simulate_data(xml_data)
    model.simulate_results= xml_data
    update_view
  end

  def get_simulate_data
    model.simulate_results_text
  end

  def set_simulate_results(recursion_depth)
    set_simulate_data(@server_connection.get_simulate_results(recursion_depth))
  end
end
