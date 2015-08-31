# Encoding: UTF-8

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
    '(' + (1..@parms.length).map { |i| "def#{i} = #{@parms[i - 1]}" }.join(', ') + ')'
  end
end
