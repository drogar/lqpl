#===============================================================================
# Much of the platform specific code should be called before Swing is touched.
# The useScreenMenuBar is an example of this.
require 'rbconfig'
require 'java'
require 'config/platform'



# End of platform specific code
#===============================================================================
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))
require 'manifest'

# Set up global error handling so that it is consistantly logged or outputed
# You will probably want to replace the puts with your application's logger
def log_the_error(exception, thread=nil)
  rexcep = exception.exception
  puts exception
  if rexcep.class == ServerProcessNotFound
    show_error_dialog("Server(s) not found",
         "LQPL requires the compiler server and emulator server to be installed on your path.
         Please download or compile these and add them to your path, e.g., in /usr/local/bin.
         See further details at http://pll.cpsc.ucalgary.ca/lqpl");
  end
  if exception.kind_of? Exception
    File.open("lqplEmulatorError.log", "w") do |f|
      f.puts exception.backtrace.join("\n")
    end
  else
    # Workaround for JRuby issue #2673, getStackTrace returning an empty array
    output_stream = java.io.ByteArrayOutputStream.new
    exception.printStackTrace(java.io.PrintStream.new(output_stream))
    puts output_stream.to_string
  end
  # add other error handling code goes here
  show_error_dialog("Application Error","The application has encountered an error and must shut down.")
end

def show_error_dialog(title, message)
  JOptionPane.show_message_dialog(nil, message, title, JOptionPane::DEFAULT_OPTION)
  System.exit(0)
end

GlobalErrorHandler.on_error {|exception, thread| log_the_error(exception, thread) }

begin
  LqplController.instance.open
rescue => e
  log_the_error(e)
end