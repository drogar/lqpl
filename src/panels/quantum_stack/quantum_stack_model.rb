class QuantumStackModel
  attr_accessor :qstack

  def initialize
    sc = ServerConnection.instance
    qstack = sc.get_qstack
  end

end
