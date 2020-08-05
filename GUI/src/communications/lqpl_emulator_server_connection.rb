require 'emulator_command_interpretor'
require 'connection'
require 'forwardable'
EMULATOR = 'lqpl-emulator'.freeze

# connect to the LQPL emulator
class LqplEmulatorServerConnection < Connection
  extend Forwardable
  def_delegators :@commander, :get_code, :get_qstack, :get_stack_translation, :get_classical_stack,
                 :get_code, :get_codepointer, :get_dump, :do_run, :do_trim, :do_simulate,
                 :do_step, :do_depth_multiple, :send_load_from_file

  def initialize(port = 9502)
    super(port)
    @connect_to = EMULATOR
    @commander = EmulatorCommandInterpretor.new(self)
    connect
  end

  def self.get_instance(port = 9502)
    super(port)
  end
end
