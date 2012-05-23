require 'exceptions/quantum_stack_invalid_create'
require 'exceptions/stack_descriptor_invalid_create'
require "model/stack_descriptor"
require "model/stack_zero"
require "model/stack_value"
require "model/stack_classical"
require "model/stack_qubit"
require "model/stack_data"

java_import com.drogar.qface.qstack.PaintMe
java_import java.awt.Dimension
java_import java.awt.BasicStroke
java_import java.awt.Color
java_import java.awt.Rectangle
java_import java.awt.Point

class QuantumStack

  include PaintMe
  attr_accessor :substacks
  attr_accessor :descriptor
  attr_accessor :name_memory_map

  def initialize(in_qstack, in_name_stack="")
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
      when StackQubit then raise QuantumStackInvalidCreate, "Qubit on stack should have substacks '#{in_qstack}" if @substacks.size == 0
      when StackClassical then raise QuantumStackInvalidCreate, "Classical element on stack should have substacks" if @substacks.size == 0
      when StackData then raise QuantumStackInvalidCreate, "Data element on stack should have substacks" if @substacks.size == 0
      end
      @memory_name_map = QuantumStack::decode_mmap in_name_stack
      @descriptor.name = @memory_name_map[@stackaddress]
    else
      @substacks = []
    end

  end

  def bottom?
    @bottom
  end

# PaintMe interface

  def paintme(g, p)
    @descriptor.paintme_at_point(g,p,Point.new(100.0,100.0))

  end

  def paintmeAtPoint(g,p,center)
    @descriptor.paintme_at_point(g,p,center)

  end

  alias :paintme_at_point :paintmeAtPoint

  # End PaintMe interface


  def self.decode_mmap(in_mmap)
    return {} if in_mmap == ""
    ret = {}
    match_list_of_maps = MMAP_PATTERN.match in_mmap
    return {} if !match_list_of_maps
    lom = match_list_of_maps[1]
    list_elem = LIST_ELEMENT_PATTERN.match(lom)
    return {} if !list_elem
    list_elems_len = list_elem[0].length
    while list_elem
      kvpairs = list_elem[1]
      kvp = KVPATTERN.match kvpairs
      return ret if !kvp
      ret[kvp[2].to_i] = kvp[1]
      kvps_len = kvp[0].length
      while kvp
        kvp = KVPATTERN.match(kvpairs[kvps_len,kvpairs.length])
        break if !kvp
        ret[kvp[2].to_i] = kvp[1]
        kvps_len += kvp[0].length
      end
      list_elem = LIST_ELEMENT_PATTERN.match(lom[list_elems_len, lom.length])
      return ret if !list_elem
      list_elems_len += list_elem[0].length
    end
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

  MMAP_PATTERN = Regexp.new /^<MMap>(.*)<\/MMap>$/
  LIST_ELEMENT_PATTERN = Regexp.new /^<map>(.*?)<\/map>/
  KVPATTERN = Regexp.new /^<kvpair><key><string>(.*?)<\/string><\/key><value><int>(\d*)<\/int><\/value><\/kvpair>/

  PATTERN = Regexp.new /^<Qstack>
      <int>-?(\d)*<\/int>  #Stackaddress ([1])
      <bool>((True)|(False))<\/bool> #on diagonal ([2])
      <substacks>(.*)<\/substacks> # the substacks ([5])
      ((<Zero\/>)|(<Value>.*<\/Value>)|(<Qubits>.*<\/Qubits>)|(<ClassicalStack>.*<\/ClassicalStack>)|(<AlgebraicData>.*<\/AlgebraicData>))  # stack descriptor [6]
      <\/Qstack>/x



end