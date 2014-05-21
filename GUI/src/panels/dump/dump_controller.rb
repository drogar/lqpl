# encoding: utf-8
# controller for displaying the dump
class DumpController < PanelController
  set_model 'DumpModel'
  set_view 'DumpView'
  set_close_action :hide

  def update_data_from_lqpl_model(lqpl_model)
    update_dump(lqpl_model.tree_depth_spinner.int_value, lqpl_model.recursion_spinner.int_value)
  end

  def update_dump(tree_depth, recursion_level)
    update_dump_data(lqpl_emulator_server_connection.get_dump(recursion_level, tree_depth))
  end

  def update_dump_data(dump_data)
    model.dump = dump_data
    update_view
  end

  def dump_data
    model.text
  end

  def update_on_lqpl_model_trim
    true
  end
end
