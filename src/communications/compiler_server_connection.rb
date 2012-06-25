require 'communications/connection'

class CompilerServerConnection < Connection
 # include Singleton

  attr_accessor :failed
  attr_accessor :failure_message

  def initialize(port=7683)
    super(port)
    @connect_to = "lqpl-compiler-server"
    connect
  end

  def self.get_instance(port=7683)
    super(port)
  end

  def compile(fname)
    @fname = fname
    @dir = File.dirname(@fname)
    File.open(fname, "r") do |f|
      qpl_file_data = f.read()
      send_and_receive_command "<qplprogram>"
      qpl_file_data.each_line(separator='\n') do |line|
        send_and_receive_command line
      end
      @connection.puts "</qplprogram>"
      @qpo_code = get_qpo_program
    end
    @qpo_code
  end

  def get_qpo_program
    accum=""
    lineno = 0
    line='CS_T'
    while line =~ /CS_/
      line = @connection.readline #until line !~ /CS_/
    end
    read_warning = (line =~ /w='YES'/)
    #puts read_warning
    @failed = false
    @failure_message = ""
    while line and line != "</qpo>\n"  and line !~ /<\/compilefail/  and !@failed do
      #puts "lineno=#{lineno}; line='#{line}'"
      @failed = true if line =~  /<compilefail/
      read_failure_message if @failed
      break if @failed
      accum += line if line !~ /(CS_)|(<qpo)|(<compilefail)|(<getFirst)/
      send_included_file(line) if line =~ /<getFirst>/
      line = @connection.readline
      lineno += 1
    end
    read_failure_message if read_warning
    return accum
  end

  def read_failure_message
    line = @connection.readline
    #puts "read_failure_message: line='#{line}'"
    @failure_message = ""
    while line and  line !~ /<\/compilefail/ and line !~ /<\/warning/ do
      @failure_message << line if line !~ /^</
      line = @connection.readline
      #puts "read_failure_message: line='#{line}'"
    end
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
    version_line = "Compiler: Version="+CompilerServerConnection::makeVersionNumber(version)
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
