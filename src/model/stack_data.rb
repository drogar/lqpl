class StackData < StackDescriptor

  PATTERN=Regexp.new /^<AlgebraicData>((<string>([\w\d_]*)<\/string><StackAddresses>((<int>(\d*)<\/int>)*)<\/StackAddresses>)+)<\/AlgebraicData>$/

  CONS_PATTERN = Regexp.new /^(<string>([\w\d_]*)<\/string><StackAddresses>((<int>(\d*)<\/int>)*)<\/StackAddresses>)/

  LIST_PATTERN = Regexp.new /^(<int>(\d*)<\/int>)/
  # match 2

  def initialize(in_string)
    matc = PATTERN.match in_string
    if matc
      @value = StackData::parse_pairs matc[1]
    else
      raise StackDescriptorInvalidCreate, in_string
    end
  end


  def length
    @value.length
  end

  def substack_labels
    @value.collect {|v| "#{v[0]}#{v[1] if [] != v[1]}"}
  end

     # PaintMe interface overrides
  def my_colour
    Color.magenta
  end


  def my_shape(point)
    Rectangle2D::Double.new(point.x-half_node_size, point.y-half_node_size, node_size, node_size)
  end
  # End PaintMe interface

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
    return [] if addresses_string == ""
    md = LIST_PATTERN.match addresses_string
    raise InvalidInput, addresses_string if !md
    rval=[md[2].to_i]
    num_found = 1
    while md
      md = LIST_PATTERN.match(addresses_string[md[1].length*num_found,addresses_string.length])
      return rval if !md
      elem = md[2].to_i
      raise InvalidInput, "StackAddress '#{elem}' duplicated for single constructor" if rval.include? elem
      rval << elem
      num_found += 1
    end
    rval
  end
end