# encoding: utf-8
# Parse the executing code xml
class ExecutingCodeParser < AbstractListPatternParser
  INSTRUCTIONS_PATTERN = /<i>(.*?)<\/i>/

  KVPAIRS_PATTERN = Regexp.new '<kvpair><key><string>(?<code_key>.*?)</string></key><value>' \
                                '<instructions>(?<instructions_list>(' +
                                INSTRUCTIONS_PATTERN.source +
                                ')*?)</instructions></value></kvpair>'

  def self.embeddable_top_level_regexp
    Regexp.new '<Code><map>(?<code_map>(' + KVPAIRS_PATTERN.source + ')*?)</map></Code>'
  end

  def parse_list
    ExecutingCodeParser.values_to_list(@md[:code_map], KVPAIRS_PATTERN, {}) do |retmap, kvpair|
      retmap[kvpair[:code_key].to_sym] =
        ExecutingCodeParser.instructions_to_list kvpair[:instructions_list]
    end
  end

  def self.instructions_to_list(instructions)
    count = 0
    values_to_list(instructions, INSTRUCTIONS_PATTERN)  do |ret, ins|
      ret << format('%3d  %s', count, ins[1])
      count += 1
    end
  end
end
