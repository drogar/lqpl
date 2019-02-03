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
    match_data = matchss.match(input)
    return nil unless match_data
    return [match_data[0]] if match_data[:all_of_it]

    inner_match(match_data, input)
  end

  def inner_match(match_data, input)
    current_position = 0
    while match_data
      return [input[0, current_position + match_data.end(0)]] if _matched_paired_start_stop?(match_data)

      current_position += match_data.end(0)
      match_data = matchss.match input[Range.new(current_position, -1)]
    end
    nil
  end

  def _matched_paired_start_stop?(match_data)
    @recurss += 1 if match_data[:strt]
    @recurss -= 1 if match_data[:stop]
    @recurss.zero?
  end
end
