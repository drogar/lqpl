class DumpSplitModel <XmlBasedModel

  def initialize(inx)
    self.dumpsplit=inx
  end

  def dumpsplit=(in_xml)
    m = DS_PATTERN.match in_xml
    raise ModelCreateError, "Invalid dump split: #{in_xml}" if !m
    @ds_text= m[0]
  end

  def text
    @ds_text
  end

  DS_PATTERN= Regexp.new /<DumpSplit>(.*?)<\/DumpSplit>/
end