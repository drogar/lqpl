require 'socket'

class Compiler
  attr :port
  
  def initialize(port=7863)
    @port = port
    begin
      @connection = TCPSocket.new "localhost", @port
    rescue Errno::ECONNREFUSED => e
      raise CompilerProcessNotFound, "There was no process found on port #{@port}. Please start 'lpql-compiler'."
    end
  end
end
