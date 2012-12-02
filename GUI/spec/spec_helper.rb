require 'rbconfig'
require 'java'

require 'simplecov'
SimpleCov.start

# Override at_exit so that rspec actually terminates properly. 
# puts did not seem to work consistently, so using err.println
# result.format! sometimes prints a line as well, but not always.

SimpleCov.at_exit do
  status= $!.is_a?(::SystemExit) ? $!.status : 0
  SimpleCov.result.format!
  java.lang.System.err.println "SimpleCov report generated, covered #{SimpleCov.result.covered_lines} lines of #{SimpleCov.result.total_lines} for a coverage of %#{SimpleCov.result.covered_percent}."
  LqplController.instance.close
  java.lang.System.exit(status)
end


$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../src"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/jruby-complete.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../devlib/java/jemmy-2.3.0.0.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/forms_rt.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/monkeybars-1.1.1.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../../out/lqpl_gui"

TEST_QP_PATH=File.expand_path(File.dirname(__FILE__))+"/../testdata/qplprograms/"

require 'manifest'

