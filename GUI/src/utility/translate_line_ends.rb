# encoding: utf-8
# change \n's to literal \n within <>
class TranslateLineEnds
  def initialize(in_string)
    @data = in_string.gsub("\n", '<\\n>')
  end

  def to_s
    @data
  end
end
