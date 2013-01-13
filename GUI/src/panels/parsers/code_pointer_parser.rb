class CodePointerParser < AbstractPatternParser
  
  def self.embeddable_top_level_regexp
    Regexp.new "<pair><string>(?<key>.*?)</string><int>(?<value>\\d*)</int></pair>"
  end
  
  def parsed_value
    [@md[:key].to_sym,@md[:value].to_i]
  end
end