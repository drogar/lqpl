require 'exceptions/quantum_stack_model_invalid_create'

class ClassicalStackModel
  include XmlDecode
  attr_accessor :classical_stack_text
  attr_accessor :classical_stack

  def classical_stack_text=(whatever)
  end

  def classical_stack_text
    cs = @classical_stack || []
    return "" if cs == []
    inside=cs.collect {|c| "#{c}"}.join("<br />")
    "<html>"+inside+"</html>"
  end

  def to_a
    @classical_stack
  end

  def classical_stack=(xml_input)
    cstack_in = CSTACK_PATTERN.match xml_input
    raise QuantumStackModelInvalidCreate, "Invalid Classical Stack: #{xml_input}" if !cstack_in
    @classical_stack = ClassicalStackModel::classical_values_to_list(cstack_in[1])
  end

  def self.classical_values_to_list(cvals)
    values_to_list cvals, CLASSICALVALUES_PATTERN, do | ret, cv|
      ret << cv[2].to_i if cv[2]
      ret << (cv[4] == "True" or cv[4] == "true") if cv[4]
    end
  end

  CSTACK_PATTERN = Regexp.new /<Cstack>(((<cint>(-?\d*)<\/cint>)|(<cbool>(((t|T)rue)|((F|f)alse))<\/cbool>))*)<\/Cstack>/
  CLASSICALVALUES_PATTERN = Regexp.new /(<cint>(-?\d*)<\/cint>)|(<cbool>(((t|T)rue)|((F|f)alse))<\/cbool>)/
end
