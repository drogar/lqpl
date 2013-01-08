class DuckMatcher
  def initialize(start,stop, entire_string)
    @matchss = Regexp.new "(?<strt>"+start+")|(?<stop>"+stop+")|(?<all_of_it>"+entire_string+")"
  end
  
  def match(input)
    @recurss = 0
    current_position = 0
    md = @matchss.match input
    return nil if !md
    return [md[0]] if md[:all_of_it]
    while md do
      return [input[0,current_position+md.end(0)]] if _matched_paired_start_stop?(md)
      current_position += md.end(0)
      md = @matchss.match input[Range.new(current_position, -1)]
    end
    return nil
  end  
  
  def _matched_paired_start_stop? md
    @recurss +=1 if md[:strt]
    @recurss -=1 if md[:stop]
    @recurss == 0
  end
end