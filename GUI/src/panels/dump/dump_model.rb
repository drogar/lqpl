# model for the Dump
class DumpModel < ApplicationModel
  attr_reader :dump

  def dump=(in_string)
    @dump = EnsureJSON.new(in_string).as_json[:dump]
  end

  def text=(_unused); end

  def text
    inside = @dump.map do |ditem|
      '<li>' + DumpModel.make_text(ditem) + '</li>'
    end
    '<html><ol>' + inside.join('') + '</ol></html>'
  end

  def self.make_text(dump_hash)
    if dump_hash.key? :dump_call
      dc = dump_hash[:dump_call]
      "Return to #{dc[:return_ep]}(#{dc[:return_label]}). CS=#{dc[:classical][:cstack]}"
    else
      dump_hash.to_s
    end
  end
end
