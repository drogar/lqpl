require 'singleton'
require 'socket'

java_import java.lang.ProcessBuilder
java_import com.drogar.lqpl.Main
java_import java.net.URLDecoder

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

    jar_path= File.expand_path(__FILE__)[Regexp.new /.*?jar!/]
    if jar_path
      jar_path=jar_path[5,jar_path.length - 18] #remove 'file:' from front, lqpl_gui.jar! from back
    else
      jar_path = File.expand_path(File.dirname(__FILE__))+"/../../"
    end
    # puts " will try from #{jar_path}"
    if !res
      begin
        # try executing from PATH first - probably not right for testing.... 
        #TODO - add flag to pick order of these.
        @process=ProcessBuilder.new(@connect_to, "").start
        sleep 0.25
        res2 = _make_connection
        raise ServerProcessNotFound if !res2
      rescue => e
        begin
          # Assume executables just below jar path 
          # Works for bundled executables.
          @process=ProcessBuilder.new("#{jar_path}bin/#{@connect_to}", "").start
          sleep 0.25
          res2 = _make_connection
          raise ServerProcessNotFound if !res2
        rescue => e1
          begin
            # assume one further .. and then over to out/bin 
            # works for rspec and cucumber 
            @process=ProcessBuilder.new("#{jar_path}../out/bin/#{@connect_to}", "").start
            sleep 0.25
            res2 = _make_connection
            raise ServerProcessNotFound if !res2
          rescue => e2
            raise ServerProcessNotFound, "There was no process found on port #{@port}. Please start '#{@connect_to}'."
          end
        end
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
