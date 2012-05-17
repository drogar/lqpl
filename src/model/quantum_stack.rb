require 'exceptions/quantum_stack_invalid_create'
require 'exceptions/stack_descriptor_invalid_create'
require "model/stack_descriptor"
require "model/stack_zero"
require "model/stack_value"
require "model/stack_classical"
require "model/stack_qubit"
require "model/stack_data"

class QuantumStack

  attr_accessor :substacks

  def initialize(in_qstack)
    @bottom = in_qstack == "<bottom/>"
    if !bottom?
      md = PATTERN.match in_qstack
      raise QuantumStackInvalidCreate, in_qstack if !md
      @stackaddress = md[1].to_i
      @on_diagonal = md[2] == "True"
      @substacks = QuantumStack::make_multiple_stacks md[5]
      @descriptor = StackDescriptor.make_instance md[6]
      case @descriptor
      when StackZero then  raise QuantumStackInvalidCreate, "Zero stack should not have substacks" if @substacks.size > 0
      when StackValue then raise QuantumStackInvalidCreate, "Value element should not have substacks" if @substacks.size > 0
      when StackQubit then raise QuantumStackInvalidCreate, "Qubit on stack should have substacks" if @substacks.size == 0
      when StackClassical then raise QuantumStackInvalidCreate, "Classical element on stack should have substacks" if @substacks.size == 0
      when StackData then raise QuantumStackInvalidCreate, "Data element on stack should have substacks" if @substacks.size == 0
      end
    else
      @substacks = []
    end

  end

  def bottom?
    @bottom
  end

  def self.make_multiple_stacks(many_stacks)
    return [] if many_stacks == ""
    bottom_pattern=Regexp.union(PATTERN, /<bottom\/>/)
    md = bottom_pattern.match many_stacks
    raise InvalidInput, many_stacks if !md
    rval=[QuantumStack.new(md[0])]
    num_found = 1
    while md
      md = bottom_pattern.match(many_stacks[md[0].length*num_found,many_stacks.length])
      return rval if !md
      rval << QuantumStack.new(md[0])
      num_found += 1
    end
    rval
  end

  PATTERN = Regexp.new /^<Qstack>
      <StackAddress><int>(\d)*<\/int><\/StackAddress>  #Stackaddress ([1])
      <bool>((True)|(False))<\/bool> #on diagonal ([2])
      <substacks>(.*)<\/substacks> # the substacks ([5])
      ((<Zero\/>)|(<Value>.*<\/Value>)|(<Qubits>.*<\/Qubits>)|(<ClassicalStack>.*<\/ClassicalStack>)|(<AlgebraicData>.*<\/AlgebraicData>))  # stack descriptor [6]
      <\/Qstack>/x
end