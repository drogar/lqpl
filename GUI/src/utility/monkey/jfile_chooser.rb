class JFileChooser
  def self.lqpl_assembled_file_opener
    chooser = JFileChooser.new()
    chooser.set_dialog_title "Load LQPO (Assembly) File"
    qpofiles = FileNameExtensionFilter.new("LQPL assembled file", ["qpo"].to_java(:string))
    chooser.set_file_filter(qpofiles)
    chooser.set_current_directory(java.io.File.new(Dir.getwd))
    chooser
  end
  
  def self.lqpl_source_file_opener
    chooser = JFileChooser.new()
    chooser.set_dialog_title "Open LQPL File for Compiling"
    qplfiles = FileNameExtensionFilter.new("LQPL source file", ["qpl"].to_java(:string))
    chooser.set_file_filter(qplfiles)
    chooser.set_current_directory(java.io.File.new(Dir.getwd))
    chooser
  end
end