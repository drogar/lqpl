class DumpSplitModel <ApplicationModel

  def initialize(inx)
    self.dumpsplit=inx
  end

  def dumpsplit=(in_xml)
    dsp = DumpSplitParser.new in_xml
    @ds_text= dsp.parsed_value
  end

  def text
    @ds_text
  end
end