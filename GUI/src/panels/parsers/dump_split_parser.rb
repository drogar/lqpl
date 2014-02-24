# encoding: utf-8
# Parse the dumps of type split
class DumpSplitParser < AbstractPatternParser
  def self.embeddable_top_level_regexp
    /<DumpSplit>(?<dumpsplit>.*)<\/DumpSplit>/
  end

  def parsed_value
    @md[:dumpsplit]
  end
end
