# encoding: utf-8
class TestingFest
  attr_reader :testing
  def initialize
    @testing = java.lang.System.get_property('com.drogar.testing.fest') == 'true'
  end

  def testing?
    testing
  end

  def not_testing?
    !testing
  end
end
