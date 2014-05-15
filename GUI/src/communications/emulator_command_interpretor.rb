# Encoding: UTF-8
require 'json'

class EmulatorCommandInterpretor

  attr_accessor :connection
  def initialize(connection)
    self.connection = connection
  end

  def send_load_from_file(depth_multiplier, fname)
    data = File.readlines(fname)
    self.connection.send_and_read_data(JSON.generate({ load_entry: 1, load_lines: data }))
  end

  def command(cmd, parms)
    self.connection.send_and_read_data(JSON.generate({ command: cmd.to_s, parameters: parms}))
  end

  def self.make_get_command(which)
    class_eval %Q{
      def get_#{which.to_s}(tree_depth = 5, recursion_depth = 1)
        command(:get_#{which.to_s},[tree_depth,recursion_depth])
      end
    }
  end

  make_get_command :qstack
  make_get_command :stack_translation
  make_get_command :classical_stack
  make_get_command :dump
  make_get_command :code
  make_get_command :codepointer

  def self.make_do_command(which, defs=[])
    def_parms = []
    def_uses = []
    defs.each_with_index do |d,i|
      def_parms << "def#{i} = #{d}"
      def_uses << "def#{i}"
    end
    def_parameters = defs == [] ? '' : '(' + def_parms.join(',') + ')'
    class_eval %Q{
      def do_#{which.to_s}#{def_parameters}
        command(:#{which.to_s},[#{def_uses.join(',')}])
      end
    }
  end

  make_do_command :run,[1]
  make_do_command :trim
  make_do_command :step,[1,1]
  make_do_command :simulate,[1]
  make_do_command :depth_multiple,[10]
end
