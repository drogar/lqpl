# Encoding: UTF-8

require 'rbconfig'
require 'java'
require 'english'
require 'simplecov'

SimpleCov.merge_timeout 1200

SimpleCov.start do
  add_filter 'GUI/spec/'
  add_filter 'spec/'
  add_group 'DevLib', 'GUI/devlib'
  add_group 'Lib', 'GUI/lib'
  add_group 'Communications', 'GUI/src/communications'
  add_group 'Config', 'GUI/src/config'
  add_group 'Dialogs', 'GUI/src/dialogs'
  add_group 'Forms', 'GUI/src/forms'
  add_group 'Main', 'GUI/src/lqpl'
  add_group 'Painting', 'GUI/src/painting'
  add_group 'Panels', 'GUI/src/panels'
  add_group 'Utility', 'GUI/src/utility'
  add_filter 'features/'
  nocov_token ':nocov:'
end

# Override at_exit so that rspec actually terminates properly.
# puts did not seem to work consistently, so using err.println
# result.format! sometimes prints a line as well, but not always.

SimpleCov.at_exit do
  status = $ERROR_INFO.is_a?(::SystemExit) ? $ERROR_INFO.status : 0
  SimpleCov.result.format!
  java.lang.System.err.println "SimpleCov report generated,
          covered #{SimpleCov.result.covered_lines} lines of
          #{SimpleCov.result.total_lines} for a coverage of
          %#{SimpleCov.result.covered_percent}."
  # SwingRunner::on_edt do
  LqplController.instance.close
  # end
  java.lang.System.exit(status)
end

project_dir_array = File.expand_path(File.dirname(__FILE__)).split(File::SEPARATOR)

project_dir = project_dir_array.reverse.drop(1).reverse.join(File::SEPARATOR)

%w(src lqpl_gui lib/java lib/ruby devlib/java devlib/ruby).each do |dir|
  $LOAD_PATH << project_dir + '/GUI/' + dir
end
$LOAD_PATH << project_dir + '/out/lqpl_gui'

# java classpath
$CLASSPATH << project_dir + '/GUI/lib/java/jruby-complete.jar'
# testing jars
%w(fest-swing-1.2 fest-assert-1.2 fest-reflect-1.2
   fest-util-1.1.2 jcip-annotations-1.0).each do |jar|
  $CLASSPATH << project_dir + '/GUI/devlib/java/' + jar + '.jar'
end

require 'fest-swing-1.2.jar'

$CLASSPATH << project_dir + '/GUI/lib/java/monkeybars-1.1.1.jar'
$CLASSPATH << project_dir + '/out/lqpl_gui'

require 'fest_testing_imports'

TEST_QP_PATH = project_dir + '/GUI/testdata/qplprograms'

require 'config/platform'

require 'manifest'

require 'component_query'
require 'drawing_extensions'
require 'raster_queries'
RSpec.configure do |conf|
  conf.mock_with :rspec do |c|
    c.syntax = :expect
  end
end

def last(arr)
  arr[arr.size - 1]
end
