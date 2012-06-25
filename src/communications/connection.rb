require 'singleton'
require 'socket'

java_import java.lang.ProcessBuilder

class Connection
  include Singleton

  attr_accessor :port
  attr_accessor :connect_to

  def initialize(port=nil)
    @port = port
    @connection = nil
    @process = nil
  end

  java_signature "boolean is_connected()"
  def connected?
    @connection != nil
  end

  def close_down
    @connection.close if connected?
    @process.destroy if @process
    @connection = nil
    @process = nil
  end

  def self.get_instance(port=nil)
    c = self.instance
    c.port = port
    c
  end

  def connect
    res = _make_connection
    if !res
      begin
        @process=ProcessBuilder.new(@connect_to, "").start
        puts "Started a process #{@connect_to}, #{@process}"
        sleep 0.25
        res2 = _make_connection
        raise ServerProcessNotFound if !res2
      rescue
        raise ServerProcessNotFound, "There was no process found on port #{@port}. Please start '#{@connect_to}'."
      end
    end
  end

  def _make_connection

    ["127.0.0.1", "::1", "localhost"].each do |addr|
      begin
        @connection = TCPSocket.new addr, @port
        return true
      rescue Errno::ECONNREFUSED => e1
        #puts "For #{addr}, exception: #{e1}"
      end
    end
    return false # made it through without returning
  end



  def send_and_receive_command(command)
    connect if !connected?
    @connection.puts command
    @connection.readline
  end
end
