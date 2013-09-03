class ExecutableCodeParser < AbstractListPatternParser
  
  def self.embeddable_top_level_regexp
    Regexp.new "<Code><map>(?<code_map>("+
       KVPAIRS_PATTERN.source+")*?)</map></Code>"
  end
  
  def parse_list
    ExecutableCodeParser::values_to_list @md[:code_map], KVPAIRS_PATTERN, {} do |retmap, kvpair|
      retmap[kvpair[:code_key].to_sym] = ExecutableCodeParser::instructions_to_list kvpair[:instructions_list]
    end
  end
  

  def self.instructions_to_list(instructions)
    count = 0
    self.values_to_list instructions, INSTRUCTIONS_PATTERN  do |ret, ins|
      ret << sprintf("%3d  %s",count,ins[1])
      count += 1
    end
  end


  INSTRUCTIONS_PATTERN = Regexp.new "<i>(.*?)</i>"

  KVPAIRS_PATTERN = Regexp.new "<kvpair><key><string>(?<code_key>.*?)</string></key><value>"+
    "<instructions>(?<instructions_list>("+INSTRUCTIONS_PATTERN.source+
    ")*?)</instructions></value></kvpair>"

end