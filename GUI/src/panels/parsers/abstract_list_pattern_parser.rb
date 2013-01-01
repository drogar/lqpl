class AbstractListPatternParser < AbstractPatternParser
  
  def initialize(instring)
    super(instring)
    @parsed_list=parse_list 
  end
  
  def parsed_value
    @parsed_list
  end
  
  def self.values_to_list(input,pattern,return_value=[])
    #return [] if !input or "" == input
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