# encoding: utf-8

require 'panels/executing_code/code_pointer'
# model to show the executing code
class ExecutingCodeModel < ApplicationModel
  attr_accessor :the_code
  attr_accessor :the_code_pointer
  attr_accessor :the_code_was_updated
  alias_method :the_code_was_updated?, :the_code_was_updated

  def the_code=(xml_string)
    ecp = ExecutingCodeParser.new xml_string
    @the_code = ecp.parsed_value
    fail ModelCreateError, 'code xml was ill-formed' unless the_code
  end

  def the_code_pointer=(xml_string)
    @the_code_pointer = CodePointer.new xml_string
    if !the_code || !the_code.key?(@the_code_pointer.qpo_method)
      @the_code_pointer = nil
    else
      @the_code_pointer.normalize(@the_code[@the_code_pointer.qpo_method].length)
    end
  end
end
