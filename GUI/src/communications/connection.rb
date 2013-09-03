require 'singleton'
require 'socket'

java_import java.lang.ProcessBuilder
java_import com.drogar.lqpl.Main
java_import java.net.URLDecoder

class Connection
  include Singleton

  attr_accessor :port
  attr_accessor :connect_to
  attr_accessor :my_path

  def initialize(port=nil)
    @port = port
    @connection = nil
    @process = nil
    _set_up_my_path
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
    
    # puts " will try from #{jar_path}"
    if !res
      begin
        # try executing from PATH first - probably not right for testing.... 
        #TODO - add flag to pick order of these.
        _start_up_the_executable_in_a_process(@connect_to)
      rescue => e
        begin
          # Assume executables just below jar path 
          # Works for bundled executables.
          _start_up_the_executable_in_a_process("#{@my_path}bin/#{@connect_to}")
        rescue => e1
          begin
            # assume one further .. and then over to out/bin 
            # works for rspec and cucumber 
            _start_up_the_executable_in_a_process("#{@my_path}../out/bin/#{@connect_to}")
          rescue => e2
            raise ServerProcessNotFound, "There was no process found on port #{@port}. Please start '#{@connect_to}'."
          end
        end
      end
    end
  end
  
  def _set_up_my_path
    @my_path= File.expand_path(__FILE__)[Regexp.new /.*?jar!/]
    if @my_path
      #:nocov:
      @my_path=@my_path[5,@my_path.length - 18] #remove 'file:' from front, lqpl_gui.jar! from back
      #:nocov:
    else
      @my_path = File.expand_path(File.dirname(__FILE__))+"/../../"
    end
  end
  
  def _start_up_the_executable_in_a_process(executable)
    @process=ProcessBuilder.new(executable, "").start
    sleep 0.25
    res2 = _make_connection
    raise ServerProcessNotFound if !res2
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
