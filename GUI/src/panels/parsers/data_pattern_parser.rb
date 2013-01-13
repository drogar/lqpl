class DataPatternParser < AbstractListPatternParser
  
  CONS_PATTERN = Regexp.new /^(<string>(?<constructor_name>[\w\d_]*)<\/string><StackAddresses>(?<addresses>(<int>(\d*)<\/int>)*)<\/StackAddresses>)/

  LIST_PATTERN = Regexp.new /^(<int>(?<address>\d*)<\/int>)/


  def self.embeddable_top_level_regexp
    Regexp.new "<AlgebraicData>(?<constructors>(<string>([\\w\\d_]*)<\/string><StackAddresses>"+
        "((<int>(\\d*)</int>)*)</StackAddresses>)+)</AlgebraicData>"
  end

  def parse_list
    constructors_string = @md[:constructors]
    DataPatternParser::values_to_list constructors_string, CONS_PATTERN do |rval, mdata|
      cons_name = mdata[:constructor_name]
      raise InvalidInput, "Constructor '#{cons_name}' duplicated in algebraic data" if rval.assoc(cons_name)
      rval << [cons_name,DataPatternParser::parse_address_list(mdata[:addresses])]
    end
  end


  def self.parse_address_list(addresses_string)
    DataPatternParser::values_to_list addresses_string, LIST_PATTERN do |rval, md|
      elem = md[:address].to_i
      raise InvalidInput, "StackAddress '#{elem}' duplicated for single constructor" if rval.include? elem
      rval << elem
    end
  end
  
  
end