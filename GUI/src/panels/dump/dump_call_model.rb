class DumpCallModel < ApplicationModel

  def initialize(inx)
    self.dumpcall=inx
  end

  def dumpcall=(in_xml)
    dcp = DumpCallParser.new in_xml
    @ret_offset=dcp.parsed_value[0]
    @ret_label=dcp.parsed_value[1]
    @cstack = ClassicalStackModel.new
    @cstack.classical_stack= dcp.parsed_value[2]
  end

  def text
    "Return to #{@ret_label}(#{@ret_offset}). CS=#{@cstack.to_a}"
  end

end
