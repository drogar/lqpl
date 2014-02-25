# encoding: utf-8
require 'singleton'
require 'socket'

java_import java.lang.ProcessBuilder
java_import com.drogar.lqpl.Main
java_import java.net.URLDecoder

# base for creating a connection
class Connection
  include Singleton

  LOCAL_CONNECTS = ['127.0.0.1', '::1', 'localhost']
  attr_accessor :port
  attr_accessor :connect_to
  attr_accessor :my_path
  attr_reader :connection

  def initialize(port = nil)
    @port = port
    @connection = nil
    @process = nil
    _set_up_my_path
  end

  java_signature 'boolean is_connected()'
  def connected?
    !connection.nil?
  end

  def close_down
    connection.close if connected?
    @process.destroy if @process
    @connection = nil
    @process = nil
  end

  def self.get_instance(port = nil)
    c = instance
    c.port = port
    c
  end

  def connection_list
    # TODO: - add flag to pick order of these.
    [@connect_to, "#{@my_path}bin/#{@connect_to}", "#{@my_path}../out/bin/#{@connect_to}"]
  end

  def connect
    errors = _make_connection
    connection_list.each do |location|
      errors = try_connecting_to location unless errors.empty?
    end
    unless errors.empty?
      fail ServerProcessNotFound,
           "There was no process found on port #{@port}. Please start '#{@connect_to}'."
    end
  end

  def try_connecting_to(location)
    begin
      _start_up_the_executable_in_a_process(location)
    rescue
      return ['Unable to connect to ' + location]
    end
    []
  end

  def send_command(command)
    connection.puts(command)
  end

  def send_and_receive_command(command)
    connect unless connected?
    connection.puts command
    connection.readline
  end

  def receive_command_data
    connection.readline
  end

  def _make_connection
    LOCAL_CONNECTS.each do |addr|
      begin
        @connection = TCPSocket.new addr, @port
        return []
      rescue Errno::ECONNREFUSED => e1
        return ["Connect refused For #{addr}, exception: #{e1}"]
      rescue SocketError  => e
        return ["Socket error for  #{addr}, exception: #{e} "]
      end
    end
  end

  private

  def _set_up_my_path
    @my_path = File.expand_path(__FILE__)[Regexp.new(/.*?jar!/)]
    if @my_path
      #:nocov:
      @my_path = @my_path[5, @my_path.length - 18]
      # remove 'file:' from front, lqpl_gui.jar! from back
      #:nocov:
    else
      @my_path = File.expand_path(File.dirname(__FILE__)) + '/../../'
    end
  end

  def _start_up_the_executable_in_a_process(executable)
    @process = ProcessBuilder.new(executable, '').start
    sleep 0.25
    res2 = _make_connection
    fail ServerProcessNotFound unless res2.empty
  end
end
