# encoding: utf-8
# Parse the zero xml
class ZeroPatternParser < AbstractPatternParser
  def self.embeddable_top_level_regexp
    /<Zero\/>/
  end

  def parsed_value
    '0' if @md
  end
end
