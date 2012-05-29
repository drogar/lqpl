require 'socket'
require 'singleton'

class Compiler
  include Singleton
  attr_accessor :port

  def initialize(port=7683)
    @port = port
    @connection = nil

  end

  def self.get_instance(port=7683)
    c = self.instance
    c.port = port
    c
  end

  def close_down
    @connection.close if connected?
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

  def compile(fname)
    @fname = fname
    @dir = File.dirname(@fname)
    File.open(fname, "r") do |f|
      qpl_file_data = f.read()
      connect if !connected?
      @connection.puts "<qplprogram>"
      @connection.readline
      qpl_file_data.each_line(separator='\n') do |line|
        @connection.puts line
        @connection.readline
      end
      @connection.puts "</qplprogram>"
      @qpo_code = get_qpo_program
    end
    @qpo_code
  end

  def get_qpo_program
    accum=""
    lineno = 0
    line = @connection.readline
    while line and line != "</qpo>\n"  and line != "</compilefail>\n"  do
      #puts "lineno=#{lineno}; line='#{line}'"
      if line =~  /<compilefail/
        @failed = true
      end
      if !(line =~ /(CS_)|(<qpo)|(<compilefail)|(<getFirst)/)
        accum += line
      end
      if line =~ /<getFirst>/
        basef = line[/(<getFirst>)(.*)(<\/getFirst>)/,2]
        if File.exists?(basef)
          f = basef
        else
          f = @dir + "/" + basef
        end
        fdata=""
        if File.exists?(f)
          File.open(f) do |incfile|
            fdata = incfile.read
          end
        else
          puts "Unable to find or open file #{f}"
          puts "Looking in #{Dir.pwd}"
        end
        @connection.puts "<file name='#{basef}'>"
        @connection.puts fdata
        @connection.puts "</file>"
      end
      line = @connection.readline
      lineno += 1
    end
    return accum
  end

  def write_qpo_file
    connect if !connected?
    @connection.puts "<sendversion />"
    version = @connection.readline
    version_line = "Compiler: Version="+Compiler::makeVersionNumber(version)
    File.open(File.dirname(@fname)+File::SEPARATOR+File.basename(@fname,".qpl")+".qpo","w+") do |f|
      f.puts version_line
      f.puts @qpo_code
    end
  end

  def self.makeVersionNumber(vstring)
    nums = vstring[/(\d+,)+\d+/].gsub(/,/,'.')
    return nums
  end
end
