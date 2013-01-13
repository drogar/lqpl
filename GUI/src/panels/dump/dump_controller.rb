class DumpController < PanelController
  set_model 'DumpModel'
  set_view 'DumpView'
  set_close_action :hide

  def set_data_from_lqpl_model(lqpl_model)
    set_dump(lqpl_model.tree_depth_spinner, lqpl_model.recursion_spinner)
  end
  
  def set_dump(tree_depth, recursion_level)
    set_dump_data(lqpl_emulator_server_connection.get_dump(tree_depth,recursion_level))
  end

  def set_dump_data(dump_data)
    model.dump= dump_data
    update_view
  end

  def get_dump_data
    model.text
  end
  
  def update_on_lqpl_model_trim
    true
  end
end
