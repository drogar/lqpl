require 'communications/connection'
COMPILER_SERVER="lqpl-compiler-server"
class CompilerServerConnection < Connection
 # include Singleton

  attr_accessor :failed
  attr_accessor :failure_message

  def initialize(port=7683)
    super(port)
    @connect_to = COMPILER_SERVER
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

  def compile_and_write_qpo(fname)
    write_qpo_file if compile(fname)
  end
  
  def get_qpo_program
    accum=""
    #lineno = 0
    line=bypass_cs_messages
    read_warning = (line =~ /w='YES'/)
    reset_failure_status
    while line and line != "</qpo>\n"  and line !~ /<\/compilefail/  and !@failed do
      #puts "lineno=#{lineno}; line='#{line}'"
      line = process_input_line line
      # @failed = line =~  /<compilefail/
      # read_failure_message if @failed
      # break if @failed
      # accum += line if line !~ command_start_regex
      # send_included_file(line) if line =~ /<getFirst>/
      # line = @connection.readline
      #lineno += 1
    end
    read_failure_message if read_warning
    return accum
  end
  
  def process_input_line line
    @failed = line =~  /<compilefail/
    read_failure_message if @failed
    return nil if @failed
    accum += line if line !~ command_start_regex
    send_included_file(line) if line =~ /<getFirst>/
    @connection.readline
  end
  
  def command_start_regex
    /(CS_)|(<qpo)|(<compilefail)|(<getFirst)/
  end
  
  def bypass_cs_messages
    line='CS_T'
    line = @connection.readline while line =~ /CS_/ #until line !~ /CS_/
    line
  end
  
  def reset_failure_status
    @failed = false
    @failure_message = ""
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

  def success_or_fail_message(file_name)
    "Compile of #{file_name} was #{failed ? 'un' : ''}successful\n"+failure_message
  end
  
  def base_file_from_line(line_input)
    basef = line_input[/(<getFirst>)(.*)(<\/getFirst>)/,2]
    File.exists?(basef) ? basef : @dir + "/" + basef
  end
  
  def send_included_file(line)
    f = base_file_from_line line
    fdata=""
    if File.exists?(f)
      File.open(f) do |incfile|
        fdata = incfile.read
      end
      write_specified_file f,fdata
    else
      @failed = true
      @failure_message =  "Unable to find or open file #{f}, Looking in #{Dir.pwd}"
    end
  end
  
  def write_specified_file(filename, filedata)
    @connection.puts "<file name='#{File.basename filename}'>"
    @connection.puts filedata
    @connection.puts "</file>"
  end

  def write_qpo_file
    connect if !connected?
    version_line = get_version_line
    File.open(object_file_name,"w+") do |f|
      f.puts version_line
      f.puts @qpo_code
    end
  end

  def object_file_name
    File.dirname(@fname)+File::SEPARATOR+File.basename(@fname,".qpl")+".qpo"
  end
  
  def get_version_line    
    @connection.puts "<sendversion />"
    version = @connection.readline
    "Compiler: Version="+CompilerServerConnection::makeVersionNumber(version)
  end
  
  def self.makeVersionNumber(vstring)
    nums = vstring[/(\d+,)+\d+/].gsub(/,/,'.')
    return nums
  end
end
