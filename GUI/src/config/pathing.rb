class Pathing
  JAR_PREFIX = 'uri:classloader:'.freeze

  attr_reader :pathing_location

  def initialize
    self.pathing_location = __dir__
  end

  def base_path
    ''
  end
end
