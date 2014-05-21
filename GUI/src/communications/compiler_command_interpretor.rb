# encoding: utf-8
require 'json'

# handle the transmision and actions of commands from the connection
class CompilerCommandInterpretor
  attr_accessor :failure_message
  attr_reader :connection
  attr_reader :object_file_name
  attr_accessor :dir, :qpo_file_name, :qpl_file_name

  def initialize(connection)
    @connection = connection
    self.dir = ''
  end

  def dir=(fname)
    @dir = File.dirname(fname)
  end

  def qpo_file_name=(file_name)
    @qpo_file_name = file_name.gsub(/\.qpl$/, '.qpo')
  end

  def qpl_file_name=(file_name)
    @qpl_file_name = File.basename(file_name, '.qpl')
  end

  def read_qpl_file(file_name)
    JSON.generate('file_name'   => file_name,
                  'qpl_program' => File.readlines(file_name))
  end

  def compile(fname)
    self.qpl_file_name = fname
    self.dir = fname
    self.qpo_file_name = fname
    self.failure_message = nil
    converse send_file(fname)
  end

  def converse(input)
    input_line = input
    while input_line
      result = JSON.parse(input_line, symbolize_names: true)
      result.each do |meth, value|
        input_line = send(meth, value)
      end
    end
  end

  def send_file(fname)
    connection.send_and_read_data(read_qpl_file(fname)) if fname
  end

  def warning(message)
    failure('Warning', message)
  end

  def compile_fail(message)
    failure('Compile Failure', message)
  end

  def illegal_input(message)
    failure('Illegal Input', message)
  end

  def failure(type, message)
    self.failure_message = "#{type}: #{message}"
    nil
  end

  def success_or_fail_message
    "Compile of #{qpl_file_name} was " \
    "#{failure_message.nil? ? '' : 'un'}successful.\n #{failure_message}"
  end

  def qpo(code)
    File.write(qpo_file_name, ([current_version_line] + code).join("\n") + "\n")
    nil
  end

  def current_version_line
    version = connection.send_and_read_data '{"command" : "send_version"}'
    'Compiler: Version=' + CompilerCommandInterpretor.make_version_number(version)
  end

  def self.make_version_number(json_string)
    JSON.parse(json_string)['version_number'].join('.')
  end
end
