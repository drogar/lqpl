# encoding: UTF-8
require 'simplecov'

SimpleCov.merge_timeout 1200

SimpleCov.start do
  add_filter 'GUI/spec/'
  add_filter '/spec/'
  add_filter '/features/'
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
  add_filter 'features'
  # rubocop:disable UselessAssignment
  nocov_token = ':nocov:'
  # rubocop:enable UselessAssignment
end

unless defined? RUBY_ENGINE && RUBY_ENGINE == 'jruby'
  abort 'Sorry - Feature tests of LQPL requires JRuby. '\
        'You appear to be running or defaulted to some other ruby engine.'
end

project_dir_array = File.expand_path(File.dirname(__FILE__)).split(File::SEPARATOR)

project_dir = project_dir_array.reverse.drop(2).reverse.join(File::SEPARATOR)

%w(src lqpl_gui lib/java lib/ruby devlib/java devlib/ruby).each do |dir|
  $LOAD_PATH << project_dir + '/GUI/' + dir
end
$LOAD_PATH << project_dir + '/out/lqpl_gui'

require 'java'

$CLASSPATH << project_dir + '/out/lqpl_gui'

# runtime jars
%w(jruby-complete monkeybars-1.1.1).each do |jar|
  $CLASSPATH << project_dir + '/GUI/lib/java/' + jar + '.jar'
end

# testing jars
%w(fest-swing-1.2 fest-assert-1.2 fest-reflect-1.2
   fest-util-1.1.2 jcip-annotations-1.0).each do |jar|
  $CLASSPATH << project_dir + '/GUI/devlib/java/' + jar + '.jar'
end

require 'fest-swing-1.2.jar'

require 'monkeybars-1.1.1.jar'

ENV['PATH'] = "#{project_dir + '/out/bin'}#{File::PATH_SEPARATOR}#{ENV['PATH']}"

require 'fest_testing_imports'

require 'manifest'

java_import java.util.regex.Pattern

java_import java.awt.Component

require 'component_query'
