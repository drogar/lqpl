class ClassicalPatternParser < AbstractListPatternParser
  
  def self.top_level_regexp
    Regexp.new /^<Classical>(?<classical_values>((<cint>(-?\d+)<\/cint>)|(<cbool>(True|False)<\/cbool>))*)<\/Classical>$/
  end
  
  LIST_PATTERN = Regexp.new /^(<cint>(?<int_value>-?\d+)<\/cint>)|(<cbool>(?<bool_value>True|False)<\/cbool>)/

  def parse_list
    values =   @md[:classical_values]  
    ClassicalPatternParser::values_to_list values, LIST_PATTERN do |ret, md|
      ret << md[:int_value].to_i if md[:int_value]
      ret << (md[:bool_value] == 'True') if md[:bool_value]
    end
  end
end