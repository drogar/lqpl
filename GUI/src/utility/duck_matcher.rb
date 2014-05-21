# encoding: utf-8

# class to work like matching but do what I want to find the data
class DuckMatcher
  attr_reader :matchss
  attr_accessor :recurss
  def initialize(start, stop, entire_string)
    @matchss = Regexp.new '(?<strt>' + start + ')|(?<stop>' + stop + ')|(?<all_of_it>' +
                          entire_string + ')'
  end

  def match(input)
    @recurss = 0
    md = matchss.match(input)
    return nil unless md
    return [md[0]] if md[:all_of_it]
    inner_match(md, input)
  end

  def inner_match(md, input)
    current_position = 0
    while md
      return [input[0, current_position + md.end(0)]] if _matched_paired_start_stop?(md)
      current_position += md.end(0)
      md = matchss.match input[Range.new(current_position, -1)]
    end
    nil
  end

  def _matched_paired_start_stop?(md)
    @recurss += 1 if md[:strt]
    @recurss -= 1 if md[:stop]
    @recurss == 0
  end
end
