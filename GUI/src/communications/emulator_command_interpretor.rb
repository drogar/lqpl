# Encoding: UTF-8
require 'json'
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
      def get_#{ which }(tree_depth = 5, recursion_depth = 1)
        command(:get_#{ which },[tree_depth,recursion_depth])
      end
    }
  end

  make_get_command :qstack
  make_get_command :stack_translation
  make_get_command :classical_stack
  make_get_command :dump
  make_get_command :code
  make_get_command :codepointer

  def self.make_do_command(which, defs = [])
    pg = ParameterGenerator.new(defs)
    class_eval %{
      def do_#{ which }#{ pg.parameters_for_definition }
        command(:#{ which },[#{ pg.parameters_for_calling }])
      end
    }
  end

  make_do_command :run, [1]
  make_do_command :trim
  make_do_command :step, [1, 1]
  make_do_command :simulate, [1]
  make_do_command :depth_multiple, [10]
end
