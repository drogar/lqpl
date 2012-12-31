class AbstractListPatternParser < AbstractPatternParser
  include XmlDecode
  
  def initialize(instring)
    super(instring)
    @parsed_list=parse_list 
  end
  
  def parsed_value
    @parsed_list
  end
  
end