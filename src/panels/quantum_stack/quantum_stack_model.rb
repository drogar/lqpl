class QuantumStackModel
  attr_accessor :qstack

  def initialize
    sc = ServerConnection.instance
    self.qstack = sc.get_qstack
  end

end
