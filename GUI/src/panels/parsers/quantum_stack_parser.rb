class QuantumStackParser < AbstractListPatternParser

  def self.embeddable_top_level_regexp
    Regexp.new "<bottom/>|"+"<Qstack><int>(?<stackaddress>-?(\\d)*)<\/int>"+
      "<bool>(?<on_diagonal>(True)|(False))</bool>"+
      "<substacks>(?<substacks>.*)</substacks>"+
      "(?<descriptor>(<Zero/>)|(<Value>.*</Value>)|(<Qubits>.*</Qubits>)|"+
      "(<Classical>.*</Classical>)|(<AlgebraicData>.*</AlgebraicData>))"+
      "</Qstack>"
  end
    
  def self.initial_qstack_regexp
    Regexp.new "^"+QuantumStackParser::embeddable_top_level_regexp.source
  end
  
  def self.sub_qstack_matcher
    DuckMatcher.new "<Qstack>", "</Qstack>", "<bottom/>"
  end
  
  def initialize(in_qst)
    if in_qst.respond_to? :substacks_string
      @md = in_qst.md
      @parsed_list = in_qst.substacks
    else
      super
    end
  end
  
  def stackaddress
    @md[:stackaddress].to_i
  end
  
  def descriptor
    @md[:descriptor]
  end
  
  def on_diagonal?
    @md[:on_diagonal] == "True"
  end
  
  def substacks_string
    @md[:substacks]
  end
  
  def bottom?
    @md[0] == "<bottom/>"
  end
  
  def substacks
    @parsed_list
  end

  def parse_list
    return [] if !substacks_string || substacks_string == ""
    QuantumStackParser::values_to_list substacks_string, QuantumStackParser::sub_qstack_matcher do |retval, qmd|
      retval << QuantumStackParser.new(qmd[0])
    end
  end
  
end