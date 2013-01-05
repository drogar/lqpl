class ParserError < RuntimeError 
end

class AbstractPatternParser
  attr_reader :md
  
  def initialize(in_string)
    @md = self.class.top_level_regexp.match in_string
    raise ParserError, "No match -#{self.class.top_level_regexp}- to -#{in_string}-" unless @md
  end
  
  def parsed?
    @md
  end
  
  def self.top_level_regexp
    self.surround_with_start_end self.embeddable_top_level_regexp
  end
  
  
  def self.embeddable_top_level_regexp
    Regexp.new /^$/
  end
  
  def self.surround_with_start_end(regexp)
    Regexp.new "^"+regexp.source+"$"
  end
end