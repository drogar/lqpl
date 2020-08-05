require 'json'
require 'parameter_generator'
# handle creation of commands for lqpl emulator
class EmulatorCommandInterpretor
  attr_accessor :connection
  def initialize(connection)
    self.connection = connection
  end

  def send_load_from_file(_depth_multiplier, fname)
    data = File.readlines(fname)
    connection.send_and_read_data(JSON.generate(load_entry: 1, load_lines: data))
  end

  def command(cmd, parms)
    connection.send_and_read_data(JSON.generate(command: cmd.to_s, parameters: parms))
  end

  def self.make_get_command(which)
    class_eval %{
      def get_#{which}(tree_depth = 5, recursion_depth = 1)
        command(:get_#{which},[tree_depth,recursion_depth])
      end
    }, __FILE__, __LINE__ - 4
  end

  %i[qstack stack_translation classical_stack dump code codepointer].each { |s| make_get_command s }

  def self.make_do_command(which, defs = [])
    pg = ParameterGenerator.new(defs)
    class_eval string_for_do_command(which, pg), __FILE__, __LINE__
  end

  def self.string_for_do_command(which, parm_generator)
    %{
      def do_#{which}#{parm_generator.parameters_for_definition}
        command(:#{which},[#{parm_generator.parameters_for_calling}])
      end
    }
  end

  [[:run, [1]], [:trim, []], [:step, [1, 1]], [:simulate, [1]], [:depth_multiple, [10]]].each { |s, defs| make_do_command s, defs }
end
