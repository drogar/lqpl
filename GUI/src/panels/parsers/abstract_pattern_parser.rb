# encoding: utf-8
# Error class for parsing
class ParserError < RuntimeError ; end

# Abstract parser
class AbstractPatternParser
  attr_reader :md
  attr_reader :parsed
  alias_method :parsed?, :parsed

  def initialize(in_string)
    @md = self.class.top_level_regexp.match in_string
    @parsed = !@md.nil?
    fail ParserError, "No match -#{self.class.top_level_regexp}- to -#{in_string}-" unless @md
  end

  def self.top_level_regexp
    surround_with_start_end embeddable_top_level_regexp
  end

  def self.embeddable_top_level_regexp
    /^$/
  end

  def self.surround_with_start_end(regexp)
    Regexp.new '^' + regexp.source + '$'
  end
end
