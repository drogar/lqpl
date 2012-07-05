require 'panels/executable_code/code_pointer'

class ExecutableCodeModel
  include XmlDecode
  attr_accessor :the_code
  attr_accessor :the_code_pointer
  attr_accessor :the_code_was_updated

  def the_code=(xml_string)
    @the_code = ExecutableCodeModel::code_xml_to_map(xml_string)
    raise QuantumStackModelInvalidCreate, "code xml was ill-formed" if !self.the_code
  end

  def the_code_pointer=(xml_string)
    @the_code_pointer = CodePointer.new xml_string
    if !the_code or !the_code.has_key?( @the_code_pointer.qpo_method)
      @the_code_pointer = nil
    else
      @the_code_pointer.normalize(@the_code[@the_code_pointer.qpo_method].length)
    end
  end

  def the_code_was_updated?
    @the_code_was_updated
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
    count = 0
    values_to_list instructions, INSTRUCTIONS_PATTERN, do |ret, ins|
      ret << sprintf("%3d  %s",count,ins[1])
      count += 1
    end
  end


  INSTRUCTIONS_PATTERN = Regexp.new /<i>(.*?)<\/i>/

  CODE_MAP_PATTERN = Regexp.new /^<Code><map>(.*)<\/map><\/Code>$/
  KVPAIRS_PATTERN = Regexp.new   /^<kvpair><key><string>(.*?)<\/string><\/key><value><instructions>(.*?)<\/instructions><\/value><\/kvpair>/



end
