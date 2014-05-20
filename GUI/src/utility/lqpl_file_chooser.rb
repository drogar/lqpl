# encoding: utf-8
require 'forwardable'
# class to wrap a jfile chooser
class LqplFileChooser
  def self.lqpl_assembled_file_opener
    opener('Load LQPO (Assembly) File', 'LQPL assembled file', 'qpo')
  end

  def self.lqpl_source_file_opener
    opener('Open LQPL File for Compiling', 'LQPL source file', 'qpl')
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
