class CodePointer
  attr_accessor :qpo_method
  attr_accessor :line_number
  def initialize(xml_string)
    cp_match = CODE_POINTER_PATTERN.match xml_string
    raise QuantumStackModelInvalidCreate, "code pointer xml was ill-formed"  if !cp_match
    @qpo_method = cp_match[1].to_sym
    @line_number = cp_match[2].to_i
  end

  def normalize(max_plus_one)
    if max_plus_one
      @line_number = [[@line_number,max_plus_one -1].min, 0].max
    else
      @line_number = 0
    end
  end

  CODE_POINTER_PATTERN = Regexp.new /^<pair><string>(.*?)<\/string><int>(\d*)<\/int><\/pair>$/
end