# class to work like matching but do what I want to find the data
class DuckMatcher
  attr_reader :matchss
  attr_accessor :recurss
  def initialize(start, stop, entire_string)
    @matchss = Regexp.new "(?<strt>#{start})|(?<stop>#{stop})|(?<all_of_it>#{entire_string})"
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
      return candidate_match(input, current_position, match_data) if matched_paired_start_stop?(match_data)

      current_position += match_data.end(0)
      match_data = new_match_data(input, current_position)
    end
    nil
  end

  def matched_paired_start_stop?(match_data)
    @recurss += 1 if match_data[:strt]
    @recurss -= 1 if match_data[:stop]
    @recurss.zero?
  end

  private

  def candidate_match(input, current_position, match_data)
    [input[0, current_position + match_data.end(0)]]
  end

  def new_match_data(input, start)
    matchss.match input[Range.new(start, -1)]
  end
end
