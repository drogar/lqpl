
class ApplicationController < Monkeybars::Controller
  # Add content here that you want to be available to all the controllers
  # in your application

  attr_reader :lqpl_emulator_server_connection
 

  java_signature "Object lqpl_emulator_server_connection()"
  def lqpl_emulator_server_connection(connect=true)
    @lqpl_emulator_server_connection = LqplEmulatorServerConnection.instance if connect && !@lqpl_emulator_server_connection 
    @lqpl_emulator_server_connection.connect if connect && !@lqpl_emulator_server_connection.connected?
    @lqpl_emulator_server_connection
  end

  def my_frame
    @__view.the_frame
  end
  
  def toggle_visibility
    visible? ? hide : show
  end

end
