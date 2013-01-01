class QuantumStackModel < XmlBasedModel

  attr_accessor :substacks
  attr_accessor :descriptor
  attr_accessor :stack_translation
  attr_accessor :stackaddress

  def quantum_stack
  end

  def quantum_stack=(in_qstack)
    raise ModelCreateError, "QuantumStack: Missing Stack Translation" if @stack_translation.nil?
    return if !in_qstack
    @preferred_size = nil
    @bottom = in_qstack == "<bottom/>"
    if !bottom?
      decode_stack_data_from_xml in_qstack

      @descriptor.name = make_name(:use_stack_address)
    else
      @substacks = []
    end
  end
  
  def decode_stack_data_from_xml(in_qstack)
    matched = SingleQSMatcher.new  in_qstack
    @stackaddress = matched.stackaddress
    @on_diagonal = matched.on_diagonal?
    @substacks = QuantumStackModel::make_multiple_stacks(matched.substacks, @stack_translation)
    @descriptor = AbstractDescriptorModel.make_instance matched.descriptor
    @descriptor.class.validate_substacks_count(@substacks)
  end
  
  def make_name(formatting)
    nm = (@stack_translation.reverse_lookup(@stackaddress)).to_s

    case formatting
    when :use_stack_address then
      nm += "(#{@stackaddress})" if nm != "#{@stackaddress}"
    end
    nm = "" if nm == "-1"
    nm
  end

  def bottom?
    @bottom
  end



  def self.make_multiple_stacks(many_stacks, st)
    return [] if many_stacks == ""
    next_stack = QuantumStackModel::get_next_qstack(many_stacks)
    raise InvalidInput, many_stacks if !next_stack
    rval = []
    q = QuantumStackModel.new
    q.stack_translation = st
    q.quantum_stack = next_stack[0]
    rval << q
    while next_stack
      next_stack = QuantumStackModel::get_next_qstack(next_stack[1])
      return rval if !next_stack
      q = QuantumStackModel.new
      q.stack_translation = st
      q.quantum_stack = next_stack[0]
      rval << q
    end
    rval
  end

  def self.get_next_qstack(multi_stacks)
    return nil if !multi_stacks or multi_stacks == ""
    if multi_stacks =~ /^<bottom\/>/
      len = 9
    else
      len = QuantumStackModel::find_end_index_for_start_in_data(multi_stacks,"<Qstack>","</Qstack>")
    end
    
    # len = 8 # length of "<Qstack>"
    # in_count = 1
    # while in_count > 0
    #   len += 1
    #   in_count += 1 if multi_stacks[len,8] == "<Qstack>"
    #   in_count -= 1 if multi_stacks[len,9] == "</Qstack>"
    # end
    # len += 9
    return [multi_stacks[0,len], multi_stacks[len, multi_stacks.length - len + 1]]
  end

  def self.find_end_index_for_start_in_data(data,start_string,end_string)
    len=start_len = start_string.size
    end_len = end_string.size
    in_count = 1
    while in_count > 0
      len +=1
      in_count += 1 if data[len,start_len] == start_string
      in_count -= 1 if data[len,end_len] == end_string
    end
    len+ end_len
  end

  class SingleQSMatcher
    def initialize(in_data)
      @md = SINGLE_QS_PATTERN.match in_data
      raise ModelCreateError, in_data if !@md
    end
    
    def stackaddress
      @md[:stackaddress].to_i
    end
    
    def descriptor
      @md[:descriptor]
    end
    
    def on_diagonal?
      @md[:on_diagonal] == "True"
    end
    
    def substacks
      @md[:substacks]
    end
  end
  
  MMAP_PATTERN = Regexp.new /^<MMap>(.*)<\/MMap>$/
  LIST_ELEMENT_PATTERN = Regexp.new /^<map>(.*?)<\/map>/
  KVPATTERN = Regexp.new /^<kvpair><key><string>(.*?)<\/string><\/key><value><int>(\d*)<\/int><\/value><\/kvpair>/


  SINGLE_QS_PATTERN = Regexp.new /^<Qstack>
      <int>(?<stackaddress>-?(\d)*)<\/int>  #Stackaddress ([1])
      <bool>(?<on_diagonal>(True)|(False))<\/bool> #on diagonal ([3])
      <substacks>(?<substacks>.*)<\/substacks> # the substacks ([6])
      (?<descriptor>(<Zero\/>)|(<Value>.*<\/Value>)|(<Qubits>.*<\/Qubits>)|(<Classical>.*<\/Classical>)|(<AlgebraicData>.*<\/AlgebraicData>))  # stack descriptor [7]
      <\/Qstack>/x


end
