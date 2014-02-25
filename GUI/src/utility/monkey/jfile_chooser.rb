# encoding: utf-8
# TODO: change to inheritance rather than monkey.
# change the java file chooser by adding a couple of options. Likely could be done via inheritance
class JFileChooser
  def self.lqpl_assembled_file_opener
    JFileChooser.opener('Load LQPO (Assembly) File', 'LQPL assembled file', 'qpo')
  end

  def self.lqpl_source_file_opener
    JFileChooser.opener('Open LQPL File for Compiling', 'LQPL source file', 'qpl')
  end

  def self.opener(title, file_description, file_extension)
    chooser = JFileChooser.new
    chooser.set_dialog_title title
    chooser_files = FileNameExtensionFilter.new(file_description, [file_extension].to_java(:string))
    chooser.set_file_filter(chooser_files)
    chooser.set_current_directory(java.io.File.new(Dir.getwd))
    chooser
  end
end
