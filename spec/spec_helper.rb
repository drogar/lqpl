require 'rbconfig'
require 'java'
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../src"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/jruby-complete.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/jemmy-2.2.7.5.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/forms_rt.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/miglayout-3.7.3.1.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../lib/java/monkeybars-1.1.1.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../out/production/qface"

require 'manifest'

def make_multi_sstacks(front,back,sstack,count)
  front+"<substacks>"+(sstack*count)+"</substacks>" + back
end
