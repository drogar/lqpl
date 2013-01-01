require 'panels/executable_code/code_pointer'

class ExecutableCodeModel
  attr_accessor :the_code
  attr_accessor :the_code_pointer
  attr_accessor :the_code_was_updated

  def the_code=(xml_string)
    ecp = ExecutableCodeParser.new xml_string
    @the_code = ecp.parsed_value
    raise ModelCreateError, "code xml was ill-formed" if !self.the_code
  end

  def the_code_pointer=(xml_string)
    @the_code_pointer = CodePointer.new xml_string
    if !the_code or !the_code.has_key?( @the_code_pointer.qpo_method)
      @the_code_pointer = nil
    else
      @the_code_pointer.normalize(@the_code[@the_code_pointer.qpo_method].length)
    end
  end

  def the_code_was_updated?
    @the_code_was_updated
  end

end
