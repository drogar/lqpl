# encoding: utf-8
module Monkeybars
  # Resolver in the monkeybars, for loading code files
  class Resolver
    IN_FILE_SYSTEM = :in_file_system
    IN_JAR_FILE = :in_jar_file

    # Returns a const value indicating if the currently executing code is being run from the
    # file system or from within a jar file.
    def self.run_location
      if File.expand_path(__FILE__) =~ /\.jar\!/
        #:nocov:#
        IN_JAR_FILE
        #:nocov:#
      else
        IN_FILE_SYSTEM
      end
    end
  end
end
# MONKEYPATCHING!!!! adding methods to Object
class Object
  def add_to_classpath(path)
    $CLASSPATH << get_expanded_path(path)
  end
  #:nocov:#
  def add_to_load_path(path)
    $LOAD_PATH << get_expanded_path(path)
  end
  #:nocov:#

  private

  def get_expanded_path(path)
    resolved_path = File.expand_path(File.dirname(__FILE__) + '/' + path.gsub('\\', '/'))
    resolved_path.gsub!('file:', '') unless resolved_path.index('.jar!')
    resolved_path.gsub!('%20', ' ')
    resolved_path
  end
end
