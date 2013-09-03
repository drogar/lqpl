class DumpSplitParser < AbstractPatternParser
  
  def self.embeddable_top_level_regexp
    Regexp.new "<DumpSplit>(?<dumpsplit>.*)</DumpSplit>"
  end
  
  def parsed_value
    @md[:dumpsplit]
  end
end