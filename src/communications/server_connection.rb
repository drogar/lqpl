require 'singleton'
require 'socket'

class ServerConnection
  include Singleton

  attr_accessor :port

  def initialize
    @port = 9502
    @connection = nil
    connect
  end

  def connect
    begin
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

  def close_down
    @connection.close if connected?
  end

  def send_load_from_file(fname)

    @fname = fname
    @dir = File.dirname(@fname)
    File.open(fname, "r") do |f|
      qpl_file_data = f.read()
      send_and_recieve_command "load #{TranslateLineEnds.new qpl_file_data}"
    end
  end

  def get_qstack(tree_depth=5, recursion_depth=1)
    [send_and_recieve_command("get qstack  #{recursion_depth} #{tree_depth}\n"),
      send_and_recieve_command("get memorymap #{recursion_depth}  #{tree_depth}\n")]
  end

  def do_step(step_size=1, depth=1)
    send_and_recieve_command "step #{step_size} #{depth}\n"
  end

  def do_run(recursion_depth=1)
    send_and_recieve_command "run #{recursion_depth}\n"
  end


  def code_pointer(recursion_depth=1)
    send_and_recieve_command "get codepointer #{recursion_depth}"
  end


  def loaded_code(recursion_depth=1)
    send_and_recieve_command "get code #{recursion_depth}"
  end
  def send_and_recieve_command(command)
    connect if !connected?
    @connection.puts command
    @connection.readline
  end
end
