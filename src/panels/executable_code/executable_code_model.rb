class ExecutableCodeModel
  attr_accessor :the_code
  attr_accessor :the_code_pointer
  attr_accessor :the_code_was_updated

  def the_code=(xml_string)
    @the_code = ExecutableCodeModel::code_xml_to_map(xml_string)
    raise QuantumStackInvalidCreate, "code xml was ill-formed" if !self.the_code
  end

  def the_code_pointer=(xml_string)
    @the_code_pointer = ExecutableCodeModel::code_pointer_xml_to_map(xml_string)
    raise QuantumStackInvalidCreate, "code pointer xml was ill-formed" if !self.the_code_pointer
    @the_code_pointer = nil if !the_code or !the_code.has_key?(@the_code_pointer.keys[0])
  end

  def the_code_was_updated?
    @the_code_was_updated
  end

  def self.code_pointer_xml_to_map(xml_string)
    return {} if !xml_string or "" == xml_string
    cp_match = CODE_POINTER_PATTERN.match xml_string
    return nil if !cp_match
    {cp_match[1].to_sym => cp_match[2].to_i}
  end

  def self.code_xml_to_map(xml_string)
    return {} if ! xml_string or "" == xml_string
    ret = {}
    map_match = CODE_MAP_PATTERN.match xml_string
    return nil if !map_match
    ExecutableCodeModel.kv_pairs_to_map map_match[1]

  end

  def self.kv_pairs_to_map(kvps)
    return {} if !kvps or kvps == ""
    ret = {}
    kvp = KVPAIRS_PATTERN.match kvps
    return ret if !kvp
    ret[kvp[1].to_sym] = ExecutableCodeModel::instructions_to_list(kvp[2])
    matched_len = kvp[0].length
    while kvp
      kvp = KVPAIRS_PATTERN.match(kvps[matched_len,kvps.length])
      return ret if !kvp
      ret[kvp[1].to_sym] = ExecutableCodeModel::instructions_to_list(kvp[2])
      matched_len += kvp[0].length
    end
  end

  def self.instructions_to_list(instructions)
    return [] if !instructions or instructions == ""
    ret = []
    ins = INSTRUCTIONS_PATTERN.match instructions
    return ret if !ins
    count = 0
    ret << sprintf("%3d  %s",count,ins[1])
    matched_len = ins[0].length
    while ins
      ins = INSTRUCTIONS_PATTERN.match(instructions[matched_len,instructions.length])
      return ret if !ins
      count += 1
      ret <<  sprintf("%3d  %s",count,ins[1])
      matched_len += ins[0].length
    end
  end


  INSTRUCTIONS_PATTERN = Regexp.new /<i>(.*?)<\/i>/

  CODE_MAP_PATTERN = Regexp.new /^<Code><map>(.*)<\/map><\/Code>$/
  KVPAIRS_PATTERN = Regexp.new   /^<kvpair><key><string>(.*?)<\/string><\/key><value><instructions>(.*?)<\/instructions><\/value><\/kvpair>/

  CODE_POINTER_PATTERN = Regexp.new /^<pair><string>(.*?)<\/string><int>(\d*)<\/int><\/pair>$/

end
