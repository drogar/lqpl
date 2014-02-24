# encoding: utf-8
# Parse the quantum stack xml
class QubitPatternParser < AbstractListPatternParser
  LIST_PATTERN = %r{<pair>(?<first_qubit>(<qz/>)|(<qo/>))(?<second_qubit>(<qz/>)|(<qo/>))</pair>}
  # match 2 and 5

  def self.embeddable_top_level_regexp
    Regexp.new '<Qubits>(?<qubits>(' + LIST_PATTERN.source + '){1,4})</Qubits>'
  end

  def parse_list
    qubit_string = @md[:qubits]
    QubitPatternParser.values_to_list(qubit_string, LIST_PATTERN)  do |rval, md|
      elem = [md[:first_qubit], md[:second_qubit]].map do |qb|
        QubitPatternParser.translate_qubit qb
      end
      fail InvalidInput, "#{elem} duplicated in qubit" if rval.include? elem
      rval << elem
    end
  end

  def self.translate_qubit(single_qubit)
    return 0 if single_qubit =~ /^<qz\/>$/
    return 1 if single_qubit =~ /^<qo\/>$/
    fail InvalidInput, "Got #{single_qubit}, expecting one of '<qz/>' or '<qo/>'"
  end
end
