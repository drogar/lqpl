require 'singleton'
require 'communications/connection'
require 'communications/compiler_server_connection'
require 'communications/lqpl_emulator_server_connection'

class ExitHandler
  include Singleton

  def handleQuitRequestWith(quit_event, quit_response)
    close_servers
    quit_response.performQuit
  end

  def close_servers
    CompilerServerConnection.instance.close_down
    LqplEmulatorServerConnection.instance.close_down
  end
end