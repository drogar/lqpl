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

  def compile(fname)
    @fname = fname
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
      @connection.readline
      @connection.puts "<sendresult />"
      @qpo_code = get_qpo_program
    end
    @qpo_code
  end

  def get_qpo_program
    accum=""
    lineno = 0
    line = @connection.readline
    while line and line != "</qpo>\n" do
      #puts "lineno=#{lineno}; line='#{line}'"
      if !(line=="CS_READY\n" or line == "CS_GOT_PROGRAM\n" or line =~ /<qpo/ or line =~ /<getFirst/)
        accum += line
      end
      if line =~ /<getFirst>/
        f = line[/(<getFirst>)(.*)(<\/getFirst>)/,2]
        if File.exists?(line[/(<getFirst>)(.*)(<\/getFirst>)/,2])
          File.open(f) do |incfile|
            fdata = incfile.read
            @connection.puts f
            @connection.puts "<file>"
            @connection.puts fdata
            @connection.puts "</file>"
          end
        end
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
    version_line = "Compiler: Version="+makeVersionNumber(version)
    File.open(File.dirname(@fname)+File::SEPARATOR+File.basename(@fname,".qpl")+".qpo","w+") do |f|
      f.puts version_line
      f.puts @qpo_code
    end
  end

  def makeVersionNumber(vstring)
    nums = vstring[/(\d+,)+\d+/].gsub(/,/,'.')
    return nums
  end
end
