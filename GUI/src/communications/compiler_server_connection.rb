require 'compiler_command_interpretor'
require 'connection'
require 'forwardable'

COMPILER_SERVER = 'lqpl-compiler-server'.freeze

# Handle connections to the compiler server
class CompilerServerConnection < Connection
  extend Forwardable
  def_delegators :@commander, :compile, :success_or_fail_message

  def initialize(port = 7683)
    super(port)
    @commander = CompilerCommandInterpretor.new(self)
    @connect_to = COMPILER_SERVER
    connect
  end

  def self.get_instance(port = 7683)
    super(port)
  end
end
