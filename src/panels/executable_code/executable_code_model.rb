class ExecutableCodeModel
  attr_accessor :the_code


  def initialize
    sc = ServerConnection.instance
    self.the_code = sc.get_executable_code
  end
end
