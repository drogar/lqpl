class ApplicationController < Monkeybars::Controller
  # Add content here that you want to be available to all the controllers
  # in your application

  attr_accessor :lqpl_emulator_server_connection

  java_signature "void lqpl_emulator_server_connection(Object)"
  def lqpl_emulator_server_connection=(sc)
    @lqpl_emulator_server_connection = sc
    @lqpl_emulator_server_connection.connect if !@lqpl_emulator_server_connection.connected?
  end

  # doing a suspenders and belt here - will assume default SC if there isn't one.
  java_signature "Object lqpl_emulator_server_connection()"
  def lqpl_emulator_server_connection
    @lqpl_emulator_server_connection = LqplEmulatorServerConnection.instance if !@lqpl_emulator_server_connection
    @lqpl_emulator_server_connection
  end


  def toggle_visibility
    visible? ? hide : show
  end

end