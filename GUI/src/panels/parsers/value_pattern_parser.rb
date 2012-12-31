class ValuePatternParser < AbstractPatternParser
  
  def self.top_level_regexp
    Regexp.new /^<Value>(<number>)?(1|1\.|1\.0|0?\.\d*|\d\.\d*e-\d\d?)(<\/number>)?<\/Value>$/
  end
  
  def parsed_value
    @md[2].to_f
  end
end