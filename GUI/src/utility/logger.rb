require 'server_process_not_found'
module LqplUtility
  # log methods
  class Logger
    SERVER_NOT_FOUND_ERROR = 'LQPL requires the compiler server and emulator server to be installed on your ' \
                             'path. Please download or compile these and add them to your path, e.g., in /usr/local/bin. ' \
                             'See further details at http://pll.cpsc.ucalgary.ca/lqpl'.freeze

    # Set up global error handling so that it is consistantly logged or outputed
    # You will probably want to replace the puts with your application's logger
    def log_the_error(exception, _thread = nil)
      log_server_not_found_error(exception)
      puts exception

      write_log_file exception

      show_error_dialog('Application Error',
                        'The application has encountered an error and must shut down.')
    end

    def log_server_not_found_error(exception)
      puts exception
      # show_error_dialog('Server(s) not found', SERVER_NOT_FOUND_ERROR) if exception.exception == ServerProcessNotFound
    end

    def write_log_file(exception)
      File.open('lqplEmulatorError.log', 'w') { |f| f.puts exception_to_string(exception) }
    end

    def exception_to_string(exception)
      exception.to_s + "\n" + exception.backtrace.join("\n")
    end

    def show_error_dialog(title, message)
      JOptionPane.show_message_dialog(nil, message, title, JOptionPane::DEFAULT_OPTION)
      System.exit(0)
    end
  end
end
