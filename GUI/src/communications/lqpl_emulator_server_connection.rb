# encoding: utf-8
require 'communications/connection'
EMULATOR = 'lqpl-emulator'

# connect to the LQPL emulator
class LqplEmulatorServerConnection < Connection
  extend Forwardable
  def_delegators :@commander, :get_code, :get_qstack, :get_stack_translation, :get_classical_stack,
                 :get_code, :get_codepointer, :get_dump, :do_run, :do_trim, :do_simulate,
                 :do_depth_multiple, :send_load_from_file

  def initialize(port = 9502)
    super(port)
    @connect_to = EMULATOR
    @commander = EmulatorCommandInterpretor.new(self)
    connect
  end

  def self.get_instance(port = 9502)
    super(port)
  end

  def do_simulate(recursion_depth = 1)
    send_and_read_data "simulate #{recursion_depth}"
  end

  def do_depth_multiple(multiplier = 10)
    send_and_read_data "setdepthmultiple #{multiplier}"
  end
end
