class ZeroPatternParser < AbstractPatternParser
  
  def self.top_level_regexp
    Regexp.new /^<Zero\/>$/
  end
  
  def parsed_value
    "0" if @md
  end
end