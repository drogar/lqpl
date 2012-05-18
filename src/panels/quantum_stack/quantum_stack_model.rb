class QuantumStackModel
  attr_accessor :qstack

  def initialize
    sc = ServerConnection.instance
    @qstack = sc.get_qstack
    puts "set up qstack model #{@qstack}"
  end

end
