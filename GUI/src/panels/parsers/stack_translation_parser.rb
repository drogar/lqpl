class StackTranslationParser < AbstractListPatternParser
  
  def self.embeddable_top_level_regexp
    Regexp.new "<MMap>(?<maps>("+LIST_PATTERN.source+")*)</MMap>"
  end
  
  def parse_list
    values = @md[:maps]
    StackTranslationParser.values_to_list values, LIST_PATTERN do |ret, mdata|
      ret << StackTranslationParser::kv_pairs_to_map(mdata[:key_value_pairs])
    end
  end
  

  def self.kv_pairs_to_map(kvps)
    self.values_to_list kvps, KVPATTERN, {} do |ret, mdkv|
      ret[mdkv[:key].to_sym] = mdkv[:value].to_i
    end
  end
  
  KVPATTERN = Regexp.new "<kvpair><key><string>(?<key>.*?)</string></key><value><int>(?<value>\\d*)</int></value></kvpair>"

  LIST_PATTERN = Regexp.new "<map>(?<key_value_pairs>("+KVPATTERN.source+")*)</map>"

end