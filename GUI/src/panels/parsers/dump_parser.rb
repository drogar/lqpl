class DumpParser < AbstractListPatternParser
  def self.embeddable_top_level_regexp
    Regexp.new "<Dump>(?<dump_list>("+DUMP_LIST_PATTERN.source+")*)</Dump>"
  end
  
  def parse_list
    DumpParser::values_to_list @md[:dump_list], DUMP_LIST_PATTERN  do |ret, dv|
      ret << DumpCallModel.new(dv[:dump_call]) if dv[:dump_call]
      ret << DumpSplitModel.new(dv[:dump_split]) if dv[:dump_split]
    end
  end
  
  DUMP_LIST_PATTERN = Regexp.new "(?<dump_call>"+DumpCallParser.embeddable_top_level_regexp.source+
      ")|(?<dump_split>"+DumpSplitParser.embeddable_top_level_regexp.source+")"
  
end