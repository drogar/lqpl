# encoding: UTF-8

# Force the drawign to start with a 4byte abgr
class DrawingExtensions
  include Drawing
  attr_accessor :gc
  attr_accessor :bi
  def initialize
    @bi = BufferedImage.new(500, 500, BufferedImage::TYPE_4BYTE_ABGR)
    @gc = @bi.create_graphics
  end
end
