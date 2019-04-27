# Set up requires and imports
class Pathing
  def self.requires_and_imports_for_lqpl(context)
    base_requires
    add_all_sibling_directories_to_load_path(__FILE__)
    # for swinging
    add_to_load_path '../lib/ruby/swingtown'
    monkeybars_resolve

    ImportJava.do_imports(context: context,
                          awt: %w[BorderLayout GridLayout Point Rectangle],
                          swing: %w[JOptionPane JFileChooser filechooser.FileNameExtensionFilter
                                    JTextArea JScrollPane BoxLayout SpinnerNumberModel],
                          lang: 'System')
    swingstart
  end

  def self.base_requires
    require 'rbconfig'
    require 'java'
    require 'config/platform_configuration'
    require 'json'
    require 'config/resolver'
    require 'config/import_java'
    require 'pry'
  end

  def self.monkeybars_resolve
    Monkeybars::Resolver.new(location: __FILE__).add_monkeybars_jar_when_in_file_system
    require 'monkeybars'
  end

  def self.swingstart
    require 'utility/swing_runner'
    require 'application_controller'
    require 'application_view'
    require 'swingtown'

    # End of Monkeybars requires
    # ==================================================================
    #
    # Add your own application-wide libraries below.  To include jars,
    # append to $CLASSPATH, or use add_to_classpath, for example:
    #
    # $CLASSPATH << File.expand_path(File.dirname(__FILE__) +
    #    "/../lib/java/swing-layout-1.0.3.jar")
    #
    # is equivalent to
    #
    # add_to_classpath "../lib/java/swing-layout-1.0.3.jar"
    #
    # There is also a helper for adding to your load path and avoiding
    # issues with file: being appended to the load path (useful for
    # JRuby libs that need your jar directory on the load path).
    #
    # add_to_load_path "../lib/java"
    #
    #
    # TODO: Write a hook methods to handle this
    # case Monkeybars::Resolver.run_location
    # when Monkeybars::Resolver::IN_FILE_SYSTEM
    #   # Files to be added only when running from the file system go here
    # when Monkeybars::Resolver::IN_JAR_FILE
    #   # Files to be added only when run from inside a jar file
    # end
  end

  def self.lqpl_requires
    require 'application_model'
    require 'lqpl/lqpl_controller'
  end

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
