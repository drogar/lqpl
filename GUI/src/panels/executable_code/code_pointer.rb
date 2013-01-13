class CodePointer < ApplicationModel
  attr_accessor :qpo_method
  attr_accessor :line_number
  def initialize(xml_string)
    begin
      cp_match = CodePointerParser.new xml_string
      @qpo_method = cp_match.parsed_value[0]
      @line_number = cp_match.parsed_value[1]
    rescue ParserError => e
      raise e unless xml_string == ""
      @qpo_method = ""
      @line_number = 0
    end
  end

  def normalize(max_plus_one)
    if max_plus_one
      @line_number = [[@line_number,max_plus_one -1].min, 0].max
    else
      @line_number = 0
    end
  end
  
  def mangle_to_selection_key
    "#{@qpo_method}--#{@line_number}"
  end
end