class QfaceController < ApplicationController
  set_model 'QfaceModel'
  set_view 'QfaceView'
  set_close_action :exit

  java_import javax.swing.JFileChooser
  java_import javax.swing.filechooser.FileNameExtensionFilter
  def file_assemble_action_performed
    chooser = JFileChooser.new()
    chooser.set_dialog_title "Open LQPL File for Assembling"
    qplfiles = FileNameExtensionFilter.new("LQPL source file", ["qpl"].to_java(:string))
    chooser.set_file_filter(qplfiles)
    chooser.set_current_directory(java.io.File.new(Dir.getwd))
    rval = chooser.show_open_dialog(nil)
    puts "rval is #{rval}"
    if rval == JFileChooser::APPROVE_OPTION
      puts "Doing approve"
      fname = chooser.get_selected_file.get_absolute_path
      File.basename(fname,".qpl")
      of = File.new(File.dirname(fname)+File.basename(fname,".qpl")+".qpo","w+")
      of.puts "Something"
      of.close
    else
      puts "Did not not not do approve."
    end
    update_view
  end

end
