class ApplicationController < Monkeybars::Controller
  # Add content here that you want to be available to all the controllers
  # in your application

  attr_accessor :server_connection

  def server_connection=(sc)
    @server_connection = sc
    @server_connection.connect if !@server_connection.connected?
  end


  def toggle_visibility
    visible? ? hide : show
  end

end