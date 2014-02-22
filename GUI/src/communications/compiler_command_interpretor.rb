# encoding: utf-8
# handle the transmision and actions of commands from the connection
class CompilerCommandInterpretor
  attr_accessor :failed
  alias_method :failed?, :failed
  attr_accessor :failure_message
  attr_reader :connection
  attr_reader :object_file_name

  COMMAND_START = /(CS_)|(<qpo)|(<compilefail)|(<getFirst)/
  def initialize(connection)
    @connection = connection
    @dir = ''
    @connection_commander = ConnectionCommander.new(connection)
  end

  def add_non_command_line(line)
    return line unless line =~ COMMAND_START
    ''
  end

  def compile(fname)
    make_object_file_name(fname)
    reset_failure_status
    File.open(fname, 'r') do |f|
      @connection_commander.send_list_of_lines(['<qplprogram>'] + f.read.lines('\n').to_a)
    end
    connection.send_command '</qplprogram>'
    qp = read_qpo_program
    write_qpo_file qp
    qp
  end

  def read_qpo_program
    accum = ''
    line = @connection_commander.bypass_messages(/CS_/)
    read_warning = (line =~ /w='YES'/)
    while line && line != "</qpo>\n"  && line !~ /<\/compilefail/  && !@failed
      line = process_input_line(line) { |checked_line| accum += add_non_command_line(checked_line) }
    end
    read_failure_message if read_warning
    accum
  end

  def process_input_line(line)
    return nil if bypass_because_of_failure(line)
    yield line
    send_included_file(line) if line =~ /<getFirst>/
    connection.receive_command_data
  end

  def bypass_because_of_failure(line)
    @failed = line =~  /<compilefail/
    read_failure_message if @failed
    @failed
  end

  def reset_failure_status
    @failed = false
    @failure_message = ''
  end

  def read_failure_message
    line = connection.receive_command_data
    @failure_message = ''
    while line &&  line !~ /<\/compilefail/ && line !~ /<\/warning/
      @failure_message << line unless line =~ /^</
      line = connection.receive_command_data
    end
  end

  def success_or_fail_message(file_name)
    "Compile of #{file_name} was #{failed ? 'un' : ''}successful\n" + failure_message
  end

  def make_object_file_name(fname)
    @dir = File.dirname(fname)
    @objfile_name = @dir + File::SEPARATOR + File.basename(fname, '.qpl') + '.qpo'
  end

  def base_file_from_line(line_input)
    basef = line_input[/(<getFirst>)(.*)(<\/getFirst>)/, 2]
    File.exists?(basef) ? basef : @dir + '/' + basef
  end

  def send_included_file(line)
    f = base_file_from_line line
    if File.exists?(f)
      @connection_commander.send_file("<file name='#{File.basename f}'>", f, '</file>')
    else
      log_failure("Unable to find or open file #{f}, Looking in #{Dir.pwd}")
    end
  end

  def log_failure(message)
    @failed = true
    @failure_message = message
  end

  def write_qpo_file(code)
    File.open(object_file_name, 'w+') do |f|
      f.puts current_version_line
      f.puts code
    end
  end

  def current_version_line
    version = connection.send_and_receive_command '<sendversion />'
    'Compiler: Version=' + CompilerCommandInterpretor.make_version_number(version)
  end

  def self.make_version_number(vstring)
    vstring[/(\d+,)+\d+/].gsub(/,/, '.')
  end
end
