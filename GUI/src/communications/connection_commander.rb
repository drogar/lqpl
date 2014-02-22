# encoding: utf-8

# low level repeatable actions for a connection
class ConnectionCommander
  attr_reader :connection
  def initialize(conn)
    @connection = conn
  end

  def bypass_messages(pattern)
    line = connection.receive_command_data
    line = connection.receive_command_data while line =~ Regexp.new(pattern)
    line
  end

  def send_list_of_lines(list)
    list.each { |l| connection.send_and_receive_command l }
  end

  def send_file(start, filename, finish)
    connection.send_command start
    connection.send_command File.read(filename)
    connection.send_command finish
  end
end
