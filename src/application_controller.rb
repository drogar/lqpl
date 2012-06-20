class ApplicationController < Monkeybars::Controller
  # Add content here that you want to be available to all the controllers
  # in your application

  attr_accessor :server_connection

  java_signature "void server_connection(Object)"
  def server_connection=(sc)
    @server_connection = sc
    @server_connection.connect if !@server_connection.connected?
  end

  # doing a suspenders and belt here - will assume default SC if there isn't one.
  java_signature "Object server_connecton()"
  def server_connection
    @server_connection = ServerConnection.instance if !@server_connection
    @server_connection
  end


  def toggle_visibility
    visible? ? hide : show
  end

end