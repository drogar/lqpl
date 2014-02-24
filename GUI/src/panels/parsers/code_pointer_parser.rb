# encoding: utf-8
# Parse the code pointer
class CodePointerParser < AbstractPatternParser
  def self.embeddable_top_level_regexp
    %r{<pair><string>(?<key>.*?)</string><int>(?<value>\d*)</int></pair>}
  end

  def parsed_value
    [@md[:key].to_sym, @md[:value].to_i]
  end
end
