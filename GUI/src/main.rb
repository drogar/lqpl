$LOAD_PATH << File.expand_path(File.dirname(__FILE__))
require 'manifest'

# Set up global error handling so that it is consistantly logged or outputed
# You will probably want to replace the puts with your application's logger
def log_the_error(exception, thread=nil)
  rexcep = exception.exception
  puts exception
  show_error_dialog("Server(s) not found",
         "LQPL requires the compiler server and emulator server to be installed on your path.
         Please download or compile these and add them to your path, e.g., in /usr/local/bin.
         See further details at http://pll.cpsc.ucalgary.ca/lqpl") if rexcep.class == ServerProcessNotFound
  
  write_log_file exception.to_s+"\n"+exception.backtrace.join("\n")
  
  show_error_dialog("Application Error","The application has encountered an error and must shut down.")
end


def write_log_file(string_data)
  File.open("lqplEmulatorError.log", "w") do |f|
    f.puts string_data
  end
end

def show_error_dialog(title, message)
  JOptionPane.show_message_dialog(nil, message, title, JOptionPane::DEFAULT_OPTION)
  System.exit(0)
end

GlobalErrorHandler.on_error {|exception, thread| log_the_error(exception, thread) }

begin
  SwingRunner.on_edt do
    LqplController.instance.open
  end
rescue => e
  log_the_error(e)
end