require 'json'

# hold, translate and provide json input as json
#   - accepts string or hash
class EnsureJSON
  attr_reader :as_json

  def initialize(input)
    raise ModelCreateError, "#{input} is not a String or Hash" unless input.is_a?(String) || input.is_a?(Hash)

    @as_json = input if input.is_a?(Hash)
    @as_json = JSON.parse(input, symbolize_names: true) if input.is_a?(String)
  end
end
