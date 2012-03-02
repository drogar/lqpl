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
    rval = chooser.show_open_dialog(nil)

    update_view
  end

end
