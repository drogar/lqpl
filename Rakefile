#require 'rawr'
require 'rake/clean'

Dir.glob("tasks/**/*.rake").each do |rake_file|
  load File.expand_path(File.dirname(__FILE__) + "/" + rake_file)
end

CLEAN.include "out/lqpl*", "out/**/*.class", "Manifest","out/**/*.rb"

CLOBBER.include "out/production"