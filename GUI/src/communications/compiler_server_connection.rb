# encoding: utf-8
require 'communications/connection'
COMPILER_SERVER='lqpl-compiler-server'

# Handle connections to the compiler server
class CompilerServerConnection < Connection
  attr_accessor :failed
  attr_accessor :failure_message

  def initialize(port = 7683)
    super(port)
    @connect_to = COMPILER_SERVER
    connect
  end

  def self.get_instance(port = 7683)
    super(port)
  end

  def compile(fname)
    @fname = fname
    @dir = File.dirname(@fname)
    File.open(fname, 'r') do |f|
      qpl_file_data = f.read
      send_and_receive_command '<qplprogram>'
      qpl_file_data.each_line('\n') do |line|
        send_and_receive_command line
      end
      @connection.puts '</qplprogram>'
      @qpo_code = read_qpo_program
    end
    @qpo_code
  end

  def compile_and_write_qpo(fname)
    write_qpo_file if compile(fname)
  end

  def read_qpo_program
    accum = ''
    line = bypass_cs_messages
    read_warning = (line =~ /w='YES'/)
    reset_failure_status
    while line && line != "</qpo>\n"  && line !~ /<\/compilefail/  && !@failed do
      line = process_input_line(line) { |checked_line| accum += add_non_command_line(checked_line)}
    end
    read_failure_message if read_warning
    accum
  end

  def add_non_command_line(line)
    line unless line =~ command_start_regex
  end

  def process_input_line(line)
    @failed = line =~  /<compilefail/
    read_failure_message if @failed
    return nil if @failed
    yield line
    send_included_file(line) if line =~ /<getFirst>/
    @connection.readline
  end

  def command_start_regex

  end

  def bypass_cs_messages
    line = 'CS_T'
    line = @connection.readline while line =~ /CS_/
    line
  end

  def reset_failure_status
    @failed = false
    @failure_message = ''
  end

  def read_failure_message
    line = @connection.readline
    @failure_message = ''
    while line &&  line !~ /<\/compilefail/ && line !~ /<\/warning/
      @failure_message << line unless line =~ /^</
      line = @connection.readline
    end
  end

  def success_or_fail_message(file_name)
    "Compile of #{file_name} was #{failed ? 'un' : ''}successful\n" + failure_message
  end

  def base_file_from_line(line_input)
    basef = line_input[/(<getFirst>)(.*)(<\/getFirst>)/, 2]
    File.exists?(basef) ? basef : @dir + '/' + basef
  end

  def send_included_file(line)
    f = base_file_from_line line
    fdata = ''
    if File.exists?(f)
      File.open(f) do |incfile|
        fdata = incfile.read
      end
      write_specified_file f, fdata
    else
      log_failure("Unable to find or open file #{f}, Looking in #{Dir.pwd}")
    end
  end

  def log_failure(message)
    @failed = true
    @failure_message = message
  end

  def write_specified_file(filename, filedata)
    @connection.puts "<file name='#{File.basename filename}'>"
    @connection.puts filedata
    @connection.puts '</file>'
  end

  def write_qpo_file
    connect unless connected?
    File.open(object_file_name, 'w+') do |f|
      f.puts current_version_line
      f.puts @qpo_code
    end
  end

  def object_file_name
    File.dirname(@fname) + File::SEPARATOR + File.basename(@fname, '.qpl') + '.qpo'
  end

  def current_version_line
    @connection.puts '<sendversion />'
    version = @connection.readline
    'Compiler: Version=' + CompilerServerConnection.make_version_number(version)
  end

  def self.make_version_number(vstring)
    vstring[/(\d+,)+\d+/].gsub(/,/, '.')
  end
end
