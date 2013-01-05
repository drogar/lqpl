class DumpCallParser < AbstractPatternParser
  
  def self.embeddable_top_level_regexp
    Regexp.new "<DumpCall><int>(?<return_offset>\\d*)</int><string>(?<return_label>[\\w\\d_]*)</string>(?<classical_stack>" +
      ClassicalPatternParser.embeddable_top_level_regexp.to_s+")</DumpCall>"
  end
  
  def parsed_value
    [@md[:return_offset],@md[:return_label], @md[:classical_stack]]
  end
  
end