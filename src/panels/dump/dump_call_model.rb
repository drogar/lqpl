require "panels/classical_stack/classical_stack_model"

class DumpCallModel

  def initialize(inx)
    self.dumpcall=inx
  end

  def dumpcall=(in_xml)
    m = DC_PATTERN.match in_xml
    raise QuantumStackModelInvalidCreate, "Invalid dump_call: #{in_xml}" if !m
    @ret_offset=m[1]
    @ret_label=m[2]
    @cstack = ClassicalStackModel.new
    @cstack.classical_stack= m[3]
  end

  def text
    "Return to #{@ret_label}(#{@ret_offset}). CS=#{@cstack.to_a}"
  end

  DC_PATTERN= Regexp.new /<DumpCall><int>(\d*)<\/int><string>([\w\d_]*)<\/string>(<Cstack>.*?<\/Cstack>)<\/DumpCall>/
end