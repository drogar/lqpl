# Base application controller - add here for anything global
class ApplicationController < Monkeybars::Controller
  java_signature 'Object lqpl_emulator_server_connection()'

  def lqpl_emulator_server_connection(connect = true)
    if connect
      @lqpl_emulator_server_connection ||= LqplEmulatorServerConnection.instance
      @lqpl_emulator_server_connection.connect unless @lqpl_emulator_server_connection.connected?
    end
    @lqpl_emulator_server_connection
  end

  def my_frame
    @__view.the_frame
  end

  def toggle_visibility
    visible? ? hide : show
  end
end
