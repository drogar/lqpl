require 'singleton'
require 'socket'

class ServerConnection
  include Singleton


  def initialize
    @port = 9502
    @connection = nil
    connect
  end

  def connect
    begin
      puts "connecting at port #{@port}"
      @connection = TCPSocket.new "127.0.0.1", @port
    rescue Errno::ECONNREFUSED => e1
      begin
        @connection = TCPSocket.new "localhost", @port
      rescue Errno::ECONNREFUSED => e
        raise ServerProcessNotFound, "There was no process found on port #{@port}. Please start 'lpql-server'."
      end
    end
  end

  def connected?
    @connection != nil
  end

  def send_load_from_file(fname)

    @fname = fname
    @dir = File.dirname(@fname)
    File.open(fname, "r") do |f|
      qpl_file_data = f.read()
      connect if !connected?
      @connection.puts "load #{TranslateLineEnds.new qpl_file_data}"
      @connection.readline
    end
  end

  def get_qstack(tree_depth=5, recursion_depth=1)
    connect if !connected?
    @connection.puts "get qstack  #{recursion_depth} #{tree_depth}\n"
    retData = @connection.readline
    @connection.puts "get memorymap #{recursion_depth}  #{tree_depth}\n"
    retMM = @connection.readline
    QuantumStack.new(retData,retMM)
  end

  def do_step
    connect if !connected?
    @connection.puts "step 1\n"
    @connection.readline
  end
end
