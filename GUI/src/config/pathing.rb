# MONKEYPATCHING!!!! adding methods to Object
class Pathing
  def self.add_to_classpath(path, file: __FILE__)
    $CLASSPATH << get_expanded_path(path, file: file) unless $CLASSPATH.include?(get_expanded_path(path, file: file))
  end

  #:nocov:#
  def self.add_to_load_path(path)
    $LOAD_PATH << get_expanded_path(path) unless $LOAD_PATH.include?(get_expanded_path(path))
  end
  #:nocov:#

  def self.add_all_sibling_directories_to_load_path(file = __FILE__)
    Dir.glob(siblings_path(file)).each do |directory|
      # File.directory? is broken in current JRuby for dirs inside jars
      # http://jira.codehaus.org/browse/JRUBY-2289
      add_a_directory_to_loadpath(directory)
    end
  end

  def self.siblings_path(file)
    unpercent_spaces(File.expand_path(File.dirname(file) + '/../**/*'))
  end

  def self.add_a_directory_to_loadpath(path)
    $LOAD_PATH << path unless path =~ /\.\w+$/
  end

  def self.get_expanded_path(path, file: __FILE__)
    resolved_path = file_path(path, file)
    remove_file_prefixes(resolved_path)
    unpercent_spaces(resolved_path)
    resolved_path
  end

  def self.file_path(path, file)
    File.expand_path(File.dirname(file) + '/' + path.tr('\\', '/'))
  end

  def self.remove_file_prefixes(path)
    path.gsub!('file:', '') unless path.index('.jar!')
  end

  def self.unpercent_spaces(path)
    path.gsub('%20', ' ')
  end
end
