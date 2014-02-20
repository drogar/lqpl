# encoding: utf-8

# handle the transmision and actions of commands from the connection
class CompilerCommandInterpretor
  COMMAND_START = /(CS_)|(<qpo)|(<compilefail)|(<getFirst)/
  def initialize(connection)
    @connection = connection
  end
end