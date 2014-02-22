# encoding: utf-8
# view  for the about dialog
class SimulateResultsParser < AbstractListPatternParser
  def random_value
    @md[:random_value]
  end

  alias_method :simulate_results, :parsed_list

  def parse_list
    SimulateResultsParser.values_to_list(@md[:triples],
                                         SimulateResultsParser.triple_pattern)  do |ret, rv|
      ret << [rv[:stackaddress], rv[:type], rv[:value]]
    end
  end

  def self.embeddable_top_level_regexp
    Regexp.new '<Simulated><double>(?<random_value>.*)</double><results>(?<triples>(' +
               SimulateResultsParser.triple_pattern.source +
               ')*)</results></Simulated>'
  end

  def self.triple_pattern
    Regexp.new '<triple><string>(?<stackaddress>.*?)</string><string>(?<type>.*?)</string>' \
               '<string>(?<value>.*?)</string></triple>'
  end
end
