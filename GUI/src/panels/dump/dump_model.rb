require "panels/dump/dump_call_model"
require "panels/dump/dump_split_model"

class DumpModel <ApplicationModel

  attr_accessor :dump
  
  def dump=(in_xml)
    dp = DumpParser.new in_xml
    @dump = dp.parsed_value
  end

  def text=(whatev)
  end

  def text
    inside = @dump.inject("") do |inner, ditem|
      inner += "<li>"+ditem.text+"</li>"
    end
    "<html><ol>"+inside+"</ol></html>"
  end


end
