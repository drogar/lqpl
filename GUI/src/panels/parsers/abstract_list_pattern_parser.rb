# encoding: utf-8
# Based on abstracgt pattern parser, parses a list.
class AbstractListPatternParser < AbstractPatternParser
  attr_reader :parsed_list
  attr_reader :parsed_value
  
  def initialize(instring)
    super(instring)
    @parsed_list = parse_list 
    @parsed_value = @parsed_list
  end
  
  def parse_list # abstract kind of method
    []
  end
  
  def self.values_to_list(input,pattern,return_value=[])
    # return [] if !input or "" == input
    match_vals = pattern.match input
    return return_value if ! match_vals
    matched_len = match_vals[0].length
    yield return_value,match_vals
    while match_vals
      match_vals = pattern.match input[matched_len, input.length]
      return return_value if ! match_vals
      matched_len += match_vals[0].length
      yield return_value,match_vals
    end
  end
end