# encoding: utf-8

require 'code_pointer'
# model to show the executing code
class ExecutingCodeModel < ApplicationModel
  attr_accessor :the_code
  attr_accessor :the_code_pointer
  attr_accessor :the_code_was_updated
  alias the_code_was_updated? the_code_was_updated

  def the_code=(in_string)
    ecp = EnsureJSON.new(in_string).as_json
    @the_code = Hash[ecp.map do |k, v|
      val = []
      v.each_with_index { |vl, i| val << format('%3d  %s', i, vl) }
      [k, val]
    end
    ]
    raise ModelCreateError, 'code xml was ill-formed' unless the_code
  end

  def the_code_pointer=(codepointer_json)
    @the_code_pointer = CodePointer.new codepointer_json
    if !the_code || !the_code.key?(@the_code_pointer.qpo_method)
      @the_code_pointer = nil
    else
      @the_code_pointer.normalize(@the_code[@the_code_pointer.qpo_method].length)
    end
  end
end
