class DataDescriptorModel < AbstractDescriptorModel

  PATTERN=Regexp.new /^<AlgebraicData>((<string>([\w\d_]*)<\/string><StackAddresses>((<int>(\d*)<\/int>)*)<\/StackAddresses>)+)<\/AlgebraicData>$/

  CONS_PATTERN = Regexp.new /^(<string>([\w\d_]*)<\/string><StackAddresses>((<int>(\d*)<\/int>)*)<\/StackAddresses>)/

  LIST_PATTERN = Regexp.new /^(<int>(\d*)<\/int>)/
  # match 2

  def initialize(in_string)
    @value = check_and_return_value(PATTERN,in_string,
      lambda { |m| DataDescriptorModel::parse_pairs m})
  end


  def length
    @value.length
  end

  def substack_labels
    @value.collect {|v| "#{v[0]}#{v[1] if [] != v[1]}"}
  end


  def self.parse_pairs(constructors_string)
    raise InvalidInput, "Must have at least one constructor" if !constructors_string or constructors_string.length == 0
    md = CONS_PATTERN.match constructors_string
    raise InvalidInput, constructors_string if !md
    rval = [[md[2], self.parse_address_list(md[3])]]
    num_found = 1
    while md
      md = CONS_PATTERN.match(constructors_string[md[1].length*num_found, constructors_string.length])
      return rval if !md
      raise InvalidInput, "Constructor '#{md[2]}' duplicated in algebraic data" if rval.assoc(md[2])
      rval << [md[2],self.parse_address_list(md[3])]
      num_found += 1
    end
    rval
  end


  def self.parse_address_list(addresses_string)
    r = values_to_list addresses_string, LIST_PATTERN do |rval, md|
      elem = md[2].to_i
      raise InvalidInput, "StackAddress '#{elem}' duplicated for single constructor" if rval.include? elem
      rval << elem
    end
    raise InvalidInput, addresses_string if r.length == 0 and addresses_string and addresses_string.length > 0
    r
  end
end