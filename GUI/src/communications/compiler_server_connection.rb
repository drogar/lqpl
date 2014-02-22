# encoding: utf-8
require 'communications/connection'
require 'forwardable'

COMPILER_SERVER = 'lqpl-compiler-server'

# Handle connections to the compiler server
class CompilerServerConnection < Connection
  extend Forwardable
  def_delegators :@commander, :compile, :failed?

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
