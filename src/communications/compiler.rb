require 'socket'

class Compiler
  attr :port

  def initialize(port=7683)
    @port = port

  end

  def connect
    begin
      @connection = TCPSocket.new "::1", @port
    rescue Errno::ECONNREFUSED => e1
      begin
        @connection = TCPSocket.new "localhost", @port
      rescue Errno::ECONNREFUSED => e
        raise CompilerProcessNotFound, "There was no process found on port #{@port}. Please start 'lpql-compiler-server'."
      end
    end
  end

  def connected?
    @connection != nil
  end
end
