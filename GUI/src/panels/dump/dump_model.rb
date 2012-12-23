require "panels/dump/dump_call_model"
require "panels/dump/dump_split_model"

class DumpModel <XmlBasedModel

  attr_accessor :dump
  
  def dump=(in_xml)
    @dump = check_and_return_value(DUMP_PATTERN,in_xml,
    lambda { |m| DumpModel::dump_values_to_list m})
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
    values_to_list dumpvals, DUMP_LIST_PATTERN  do |ret, dv|
      ret << DumpCallModel.new(dv[1]) if dv[1]
      ret << DumpSplitModel.new(dv[2]) if dv[2]
    end
  end
  DUMP_LIST_PATTERN = Regexp.new /(<DumpCall>.*?<\/DumpCall>)|(<DumpSplit>.*?<\/DumpSplit>)/
  DUMP_PATTERN= Regexp.new /<Dump>(.*?)<\/Dump>/
end
