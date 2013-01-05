class QubitPatternParser < AbstractListPatternParser
  
  LIST_PATTERN = Regexp.new "<pair>(?<first_qubit>(<qz/>)|(<qo/>))(?<second_qubit>(<qz/>)|(<qo/>))</pair>"
  # match 2 and 5
  
  def self.embeddable_top_level_regexp
    Regexp.new "<Qubits>(?<qubits>("+
        LIST_PATTERN.source+"){1,4})<\/Qubits>"
  end
    
  
  def parse_list
    qubit_string = @md[:qubits]
    QubitPatternParser::values_to_list qubit_string, LIST_PATTERN  do |rval,md|
      elem = [md[:first_qubit], md[:second_qubit]].collect {|qb| QubitPatternParser::translate_qubit qb}
      raise InvalidInput, "#{elem} duplicated in qubit" if rval.include? elem
      rval << elem
    end
  end
  
  def self.translate_qubit(single_qubit)
   return 0 if single_qubit =~/^<qz\/>$/
   return 1 if single_qubit =~/^<qo\/>$/
   raise InvalidInput, "Got #{single_qubit}, expecting one of '<qz/>' or '<qo/>'"
  end
end