require 'rbconfig'
require 'java'
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../src"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/jruby-complete.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../devlib/java/jemmy-2.2.7.5.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/forms_rt.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/monkeybars-1.1.1.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../out/production/lqpl_gui"

require 'manifest'


