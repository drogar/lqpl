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
    chooser.set_file_filter(chooser_file_filter(file_description, file_extension))
    chooser.set_current_directory(current_directory)
    chooser
  end

  def self.open_and_compile(frame, model)
    chooser = lqpl_source_file_opener
    if chooser.show_open_dialog(frame) == JFileChooser::APPROVE_OPTION
      model.compile(chooser.get_selected_file)
    else
      model.messages_text = 'Compile action cancelled.'
    end
  end

  def self.open_and_load_qpl(model)
    chooser = LqplFileChooser.lqpl_assembled_file_opener
    if chooser.show_open_dialog(nil) == JFileChooser::APPROVE_OPTION
      model.load_and_enable! chooser.selected_file
      return true
    end

    model.messages_text = 'QPO file load cancelled.'
    false
  end

  def self.chooser_file_filter(file_description, file_extension)
    FileNameExtensionFilter.new(file_description, [file_extension].to_java(:string))
  end

  def self.current_directory
    java.io.File.new(Dir.getwd)
  end
end
