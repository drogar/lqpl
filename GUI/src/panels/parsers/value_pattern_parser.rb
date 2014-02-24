# encoding: utf-8
# Parse the values xml
class ValuePatternParser < AbstractPatternParser
  def self.embeddable_top_level_regexp
    %r{<Value>(<number>)?(1|1\.|1\.0|0?\.\d*|\d\.\d*e-\d\d?)(</number>)?</Value>}
  end

  def parsed_value
    @md[2].to_f
  end
end
