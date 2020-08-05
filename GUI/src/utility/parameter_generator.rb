# Class to generate appropriate number of parameters with defaults when generating functions
class ParameterGenerator
  def initialize(parms = [])
    @parms = parms
  end

  def parameters_for_calling
    return '' if @parms == []

    (1..@parms.length).map { |i| "def#{i}" }.join(', ')
  end

  def parameters_for_definition
    return '' if @parms == []

    '(' + joined_parameter_strings + ')'
  end

  private

  def joined_parameter_strings
    (1..@parms.length).map { |i| definition_parameter_string(i) }.join(', ')
  end

  def definition_parameter_string(index)
    "def#{index} = #{@parms[index - 1]}"
  end
end
