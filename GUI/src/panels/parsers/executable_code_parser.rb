class ExecutableCodeParser < AbstractListPatternParser
  
  def self.top_level_regexp
    Regexp.new /^<Code><map>(?<code_map>(<kvpair><key><string>(.*?)<\/string><\/key><value><instructions>(<i>(.*?)<\/i>)*?<\/instructions><\/value><\/kvpair>)*?)<\/map><\/Code>$/
  end
  
  def parse_list
    ExecutableCodeParser::values_to_list @md[:code_map], KVPAIRS_PATTERN, {} do |retmap, kvpair|
      retmap[kvpair[:code_key].to_sym] = ExecutableCodeParser::instructions_to_list kvpair[:instructions_list]
    end
  end
    # 
    # def self.kv_pairs_to_map(kvps)
    #   return {} if !kvps or kvps == ""
    #   ret = {}
    #   kvp = KVPAIRS_PATTERN.match kvps
    #   return ret if !kvp
    #   ret[kvp[1].to_sym] = ExecutableCodeModel::instructions_to_list(kvp[2])
    #   matched_len = kvp[0].length
    #   while kvp
    #     kvp = KVPAIRS_PATTERN.match(kvps[matched_len,kvps.length])
    #     return ret if !kvp
    #     ret[kvp[1].to_sym] = ExecutableCodeModel::instructions_to_list(kvp[2])
    #     matched_len += kvp[0].length
    #   end
    # end

  def self.instructions_to_list(instructions)
    count = 0
    self.values_to_list instructions, INSTRUCTIONS_PATTERN  do |ret, ins|
      ret << sprintf("%3d  %s",count,ins[1])
      count += 1
    end
  end


  INSTRUCTIONS_PATTERN = Regexp.new /<i>(.*?)<\/i>/

  KVPAIRS_PATTERN = Regexp.new   /^<kvpair><key><string>(?<code_key>.*?)<\/string><\/key><value><instructions>(?<instructions_list>(<i>(.*?)<\/i>)*?)<\/instructions><\/value><\/kvpair>/

end