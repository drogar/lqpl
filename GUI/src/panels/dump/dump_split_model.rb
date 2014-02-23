# encoding: utf-8
# model for the Dumps of type 'Split'
class DumpSplitModel < ApplicationModel
  attr_reader :ds_text
  alias_method :text, :ds_text

  def initialize(inx)
    self.dumpsplit = inx
  end

  def dumpsplit=(in_xml)
    dsp = DumpSplitParser.new in_xml
    @ds_text = dsp.parsed_value
  end
end
