require 'simplecov'
SimpleCov.start

require 'rbconfig'
require 'java'
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../src"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/jruby-complete.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../devlib/java/jemmy-2.3.0.0.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/forms_rt.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/monkeybars-1.1.1.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../../out/lqpl_gui"

TEST_QP_PATH=File.expand_path(File.dirname(__FILE__))+"/../testdata/qplprograms/"
require 'manifest'


