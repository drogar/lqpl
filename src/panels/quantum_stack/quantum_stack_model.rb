require 'exceptions/quantum_stack_model_invalid_create'
require 'exceptions/stack_descriptor_model_invalid_create'
require "panels/quantum_stack/descriptor/abstract_descriptor_model"
require "panels/quantum_stack/descriptor/zero_descriptor_model"
require "panels/quantum_stack/descriptor/value_descriptor_model"
require "panels/quantum_stack/descriptor/classical_descriptor_model"
require "panels/quantum_stack/descriptor/qubit_descriptor_model"
require "panels/quantum_stack/descriptor/data_descriptor_model"

class QuantumStackModel

  attr_accessor :substacks
  attr_accessor :descriptor
  attr_accessor :name_memory_map

  def initialize(qstack=nil,mmap="")
    self.quantum_stack=[qstack,mmap]
  end

  def quantum_stack
    self
  end

  def quantum_stack=(pair_of_qstack_and_name_map)
    in_qstack = pair_of_qstack_and_name_map[0]
    in_name_stack=pair_of_qstack_and_name_map[1]
    return if !in_qstack
    @preferred_size = nil
    @bottom = in_qstack == "<bottom/>"
    if !bottom?
      md = SINGLE_QS_PATTERN.match in_qstack
      raise QuantumStackModelInvalidCreate, in_qstack if !md
      @stackaddress = md[1].to_i
      @on_diagonal = md[3] == "True"
      @substacks = QuantumStackModel::make_multiple_stacks(md[6], in_name_stack)
      @descriptor = AbstractDescriptorModel.make_instance md[7]
      case @descriptor
      when ZeroDescriptorModel then  raise QuantumStackModelInvalidCreate, "Zero stack should not have substacks" if @substacks.size > 0
      when ValueDescriptorModel then raise QuantumStackModelInvalidCreate, "Value element should not have substacks" if @substacks.size > 0
      when QubitDescriptorModel then raise QuantumStackModelInvalidCreate, "Qubit on stack should have substacks '#{in_qstack}" if @substacks.size == 0
      when ClassicalDescriptorModel then raise QuantumStackModelInvalidCreate, "Classical element on stack should have substacks" if @substacks.size == 0
      when DataDescriptorModel then raise QuantumStackModelInvalidCreate, "Data element on stack should have substacks" if @substacks.size == 0
      end
      @name_memory_map = QuantumStackModel::decode_mmap in_name_stack
      @descriptor.name = @name_memory_map[@stackaddress]
    else
      @substacks = []
    end

  end

  def bottom?
    @bottom
  end

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
      ret.merge!(QuantumStackModel::kv_pairs_to_map(list_elem[1]))
      list_elem = LIST_ELEMENT_PATTERN.match(lom[list_elems_len, lom.length])
      return ret if !list_elem
      list_elems_len += list_elem[0].length
    end
  end

  def self.kv_pairs_to_map(kvps)
    return {} if !kvps or kvps == ""
    ret = {}
    kvp = KVPATTERN.match kvps
    return ret if !kvp
    ret[kvp[2].to_i] = kvp[1]
    matched_len = kvp[0].length
    while kvp
      kvp = KVPATTERN.match(kvps[matched_len,kvps.length])
      return ret if !kvp
      ret[kvp[2].to_i] = kvp[1]
      matched_len += kvp[0].length
    end
  end

  def self.make_multiple_stacks(many_stacks,in_name_stack="")
    return [] if many_stacks == ""
    next_stack = QuantumStackModel::get_next_qstack(many_stacks)
    raise InvalidInput, many_stacks if !next_stack
    rval=[QuantumStackModel.new(next_stack[0],in_name_stack)]
    while next_stack
      next_stack = QuantumStackModel::get_next_qstack(next_stack[1])
      return rval if !next_stack
      rval << QuantumStackModel.new(next_stack[0],in_name_stack)
    end
    rval
  end

  def self.get_next_qstack(multi_stacks)
    return nil if !multi_stacks or multi_stacks == ""
    if multi_stacks =~ /^<bottom\/>/
      return ["<bottom/>",multi_stacks[9,multi_stacks.length]]
    end
    if multi_stacks =~ /<Qstack/
      len = 8 # length of "<Qstack>"
      in_count = 1
      while in_count > 0
        len += 1
        in_count += 1 if multi_stacks[len,8] == "<Qstack>"
        in_count -= 1 if multi_stacks[len,9] == "</Qstack>"
      end
      len += 9
      return [multi_stacks[0,len], multi_stacks[len, multi_stacks.length - len + 1]]
    end
  end

  MMAP_PATTERN = Regexp.new /^<MMap>(.*)<\/MMap>$/
  LIST_ELEMENT_PATTERN = Regexp.new /^<map>(.*?)<\/map>/
  KVPATTERN = Regexp.new /^<kvpair><key><string>(.*?)<\/string><\/key><value><int>(\d*)<\/int><\/value><\/kvpair>/


  SINGLE_QS_PATTERN = Regexp.new /^<Qstack>
      <int>(-?(\d)*)<\/int>  #Stackaddress ([1])
      <bool>((True)|(False))<\/bool> #on diagonal ([3])
      <substacks>(.*)<\/substacks> # the substacks ([6])
      ((<Zero\/>)|(<Value>.*<\/Value>)|(<Qubits>.*<\/Qubits>)|(<ClassicalStack>.*<\/ClassicalStack>)|(<AlgebraicData>.*<\/AlgebraicData>))  # stack descriptor [7]
      <\/Qstack>/x


end
