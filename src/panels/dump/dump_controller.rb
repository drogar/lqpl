class DumpController < ApplicationController
  set_model 'DumpModel'
  set_view 'DumpView'
  set_close_action :hide

  def set_dump(tree_depth, recursion_level)
    set_dump_data(@server_connection.get_dump(tree_depth,recursion_level))
  end
  def set_dump_data(dump_data)
    model.dump= dump_data
    update_view
  end

  def get_dump_data
    model.text
  end
end
