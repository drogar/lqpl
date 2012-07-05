require "panels/dump/dump_call_model"
require "panels/dump/dump_split_model"

class DumpModel
  include XmlDecode
  attr_accessor :dump
  def dump=(in_xml)

    m = DUMP_PATTERN.match in_xml
    raise QuantumStackModelInvalidCreate, "Invalid dump: #{in_xml}" if !m
    @dump = DumpModel.dump_values_to_list m[1]
  end

  def text=(whatev)
  end

  def text
    inside = @dump.inject("") do |inner, ditem|
      inner += "<li>"+ditem.text+"</li>"
    end
    "<html><ol>"+inside+"</ol></html>"
  end

  def self.dump_values_to_list(dumpvals)
    values_to_list dumpvals, DUMPVALUES_PATTERN, do |ret, dv|
      ret << DumpCallModel.new(dv[1]) if dv[1]
      ret << DumpSplitModel.new(dv[2]) if dv[2]
    end
  end
  DUMPVALUES_PATTERN = Regexp.new /(<DumpCall>.*?<\/DumpCall>)|(<DumpSplit>.*?<\/DumpSplit>)/
  DUMP_PATTERN= Regexp.new /<Dump>(.*?)<\/Dump>/
end
