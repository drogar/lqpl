# encoding: utf-8
require 'singleton'
require 'communications/connection'
require 'communications/compiler_server_connection'
require 'communications/lqpl_emulator_server_connection'

# Java exit handler
class ExitHandler
  include Singleton

  def handle_quit_request_with(_quit_event, quit_response)
    close_servers
    quit_response.performQuit
  end

  def close_servers
    CompilerServerConnection.instance.close_down
    LqplEmulatorServerConnection.instance.close_down
  end
end
