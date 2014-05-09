# encoding: utf-8
require 'communications/connection'
EMULATOR = 'lqpl-emulator'

# connect to the LQPL emulator
class LqplEmulatorServerConnection < Connection
  def initialize(port = 9502)
    super(port)
    @connect_to = EMULATOR
    connect
  end

  def self.get_instance(port = 9502)
    super(port)
  end

  def send_load_from_file(depth_multiplier, fname)
    @fname = fname
    @dir = File.dirname(@fname)
    File.open(fname, 'r') do |f|
      qpl_file_data = f.read
      send_and_get_data "load #{depth_multiplier} #{TranslateLineEnds.new qpl_file_data}"
    end
  end

  def get_qstack(tree_depth = 5, recursion_depth = 1)
    send_and_get_data("get qstack  #{recursion_depth} #{tree_depth}\n")
  end

  def get_stack_translation(tree_depth = 5, recursion_depth = 1)
    send_and_get_data("get memorymap #{recursion_depth}  #{tree_depth}\n")
  end

  def get_classical_stack(tree_depth = 5, recursion_depth = 1)
    send_and_get_data("get classicalstack #{recursion_depth}  #{tree_depth}\n")
  end

  def get_dump(tree_depth = 5, recursion_depth = 1)
    send_and_get_data("get dump #{recursion_depth}  #{tree_depth}\n")
  end

  def do_step(step_size = 1, recursion_depth = 1)
    send_and_get_data "step #{step_size} #{recursion_depth}\n"
  end

  def do_run(recursion_depth = 1)
    send_and_get_data "run #{recursion_depth}\n"
  end

  def do_trim
    send_and_get_data "trim\n"
  end

  def code_pointer(recursion_depth = 1)
    send_and_get_data "get codepointer #{recursion_depth}"
  end

  def loaded_code(recursion_depth =  1)
    send_and_get_data "get code #{recursion_depth}"
  end

  def get_simulate_results(recursion_depth = 1)
    send_and_get_data "simulate #{recursion_depth}"
  end

  def send_set_depth_multiplier(multiplier = 10)
    send_and_get_data "setdepthmultiple #{multiplier}"
  end
end
