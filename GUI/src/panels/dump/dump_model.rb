# encoding: utf-8

require 'panels/dump/dump_call_model'
require 'panels/dump/dump_split_model'

# model for the Dump
class DumpModel < ApplicationModel
  attr_accessor :dump

  def dump=(in_string)
    @dump = EnsureJSON.new(in_string).as_json[:dump]
  end

  def text=(_)
  end

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
