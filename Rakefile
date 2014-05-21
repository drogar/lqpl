require 'rubocop/rake_task'
require 'rake/clean'

if not (defined? RUBY_ENGINE && RUBY_ENGINE == 'jruby')
  abort 'Sorry - Building LQPL requires JRuby. You appear to be running or defaulted to some other ruby engine.'
end

Rubocop::RakeTask.new

Dir.glob("tasks/**/*.rake").each do |rake_file|
   load File.expand_path(File.dirname(__FILE__) + "/" + rake_file)
end



# Dir.glob("tasks/**/*.rake").each do |rake_file|
#   load File.expand_path(File.dirname(__FILE__) + "/" + rake_file)
# end

CLEAN.include "out/**/*.class", "Manifest","out/**/*.rb", "out/lqpl*"

CLOBBER.include "out"