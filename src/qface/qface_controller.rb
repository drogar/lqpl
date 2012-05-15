class QfaceController < ApplicationController
  set_model 'QfaceModel'
  set_view 'QfaceView'
  set_close_action :exit

  java_import javax.swing.JFileChooser
  java_import javax.swing.filechooser.FileNameExtensionFilter

  def file_compile_action_performed
    chooser = JFileChooser.new()
    chooser.set_dialog_title "Open LQPL File for Compiling"
    qplfiles = FileNameExtensionFilter.new("LQPL source file", ["qpl"].to_java(:string))
    chooser.set_file_filter(qplfiles)
    puts "Setting curr dir to #{Dir.getwd}"
    chooser.set_current_directory(java.io.File.new(Dir.getwd))
    rval = chooser.show_open_dialog(nil)
    if rval == JFileChooser::APPROVE_OPTION
      fname = chooser.get_selected_file.get_absolute_path
      cmp = Compiler.new
      cmp.compile fname
      cmp.write_qpo_file
    else
      puts "Did not do approve."
    end
    update_view
  end

  def file_load_action_performed
    chooser = JFileChooser.new()
    chooser.set_dialog_title "Load LQPO (Assembly) File"
    qpofiles = FileNameExtensionFilter.new("LQPL assembled file", ["qpo"].to_java(:string))
    chooser.set_file_filter(qpofiles)
    puts "Setting curr dir to #{Dir.getwd}"
    chooser.set_current_directory(java.io.File.new(Dir.getwd))
    rval = chooser.show_open_dialog(nil)
    if rval == JFileChooser::APPROVE_OPTION
      fname = chooser.get_selected_file.get_absolute_path
      server = ServerConnection.instance
      server.send_load_from_file fname
      model.control_panel_visible = true
      QuantumStackController.instance.open
    else
      puts "Did not do approve."
    end
    update_view
  end

end
