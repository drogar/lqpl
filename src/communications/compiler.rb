require 'socket'
require 'singleton'

class Compiler
  include Singleton
  attr_accessor :port
  attr_accessor :failed
  attr_accessor :failure_message

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
    @failed = false
    @failure_message = ""
    while line and line != "</qpo>\n"  and line != "</compilefail>\n"  and !@failed do
      #puts "lineno=#{lineno}; line='#{line}'"
      @failed = true if line =~  /<compilefail/
      @failure_message = line if @failed
      accum += line if !(line =~ /(CS_)|(<qpo)|(<compilefail)|(<getFirst)/)
      send_included_file(line) if line =~ /<getFirst>/
      line = @connection.readline
      lineno += 1
    end
    return accum
  end

  def send_included_file(line)
    basef = line[/(<getFirst>)(.*)(<\/getFirst>)/,2]
    f = File.exists?(basef) ? basef : @dir + "/" + basef
    fdata=""
    if File.exists?(f)
      File.open(f) do |incfile|
        fdata = incfile.read
      end
      @connection.puts "<file name='#{basef}'>"
      @connection.puts fdata
      @connection.puts "</file>"
    else
      @failed = true
      @failure_message =  "Unable to find or open file #{f}, Looking in #{Dir.pwd}"
    end
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
