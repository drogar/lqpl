class ParserError < RuntimeError 
end

class AbstractPatternParser
  def initialize(in_string)
    @md = self.class.top_level_regexp.match in_string
    raise ParserError, "No match -#{self.class.top_level_regexp}- to -#{in_string}-" unless @md
  end
  
  def parsed?
    @md
  end
  
  def self.top_level_regexp
    Regexp.new /^$/
  end
  
end