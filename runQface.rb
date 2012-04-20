$LOAD_PATH << "/Users/gilesb/programming/JRubyProjects/qface/out/production/Qface"
$LOAD_PATH << "/Users/gilesb/programming/JRubyProjects/qface/lib/java"


require 'java'

$CLASSPATH << "/Users/gilesb/programming/JRubyProjects/qface/out/production/Qface"
$CLASSPATH << "/Users/gilesb/programming/JRubyProjects/qface/lib/java"



require "monkeybars-1.1.1.jar"
require "forms_rt.jar"


ENV['PATH'] = "#{File.expand_path(File.dirname(__FILE__) + '/../../bin')}#{File::PATH_SEPARATOR}#{ENV['PATH']}"

QUFACE = java_import com.drogar.qface.Main
#args = [""].to_java(:string)
begin
  QUFACE.main([])
rescue
  puts "Had a problem"
end