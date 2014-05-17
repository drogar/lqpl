# Encoding: UTF-8
# hold, translate and provide json input as json
#   - accepts string or hash
class EnsureJSON

  attr_reader :as_json
  def initialize(input)
    unless input.kind_of?(String) || input.kind_of?(Hash)
      fail ModelCreateError, "#{input} is not a String or Hash"
    end
    @as_json = input if input.kind_of?(Hash)
    @as_json = JSON.parse(input, symbolize_names: true) if input.kind_of?(String)
  end
end
