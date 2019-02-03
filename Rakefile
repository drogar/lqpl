require 'rubocop/rake_task'
require 'rake/clean'

abort 'Sorry - Building LQPL requires JRuby. You are using some other ruby engine.' unless defined? RUBY_ENGINE && RUBY_ENGINE == 'jruby'

RuboCop::RakeTask.new

Dir.glob('tasks/**/*.rake').each do |rake_file|
  load File.expand_path(File.dirname(__FILE__) + '/' + rake_file)
end

# Dir.glob("tasks/**/*.rake").each do |rake_file|
#   load File.expand_path(File.dirname(__FILE__) + "/" + rake_file)
# end

CLEAN.include 'out/**/*.class', 'Manifest', 'out/**/*.rb', 'out/lqpl*'

CLOBBER.include 'out'
