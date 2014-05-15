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

  def do_simulate(recursion_depth = 1)
    send_and_read_data "simulate #{recursion_depth}"
  end

  def do_depth_multiple(multiplier = 10)
    send_and_read_data "setdepthmultiple #{multiplier}"
  end
end
