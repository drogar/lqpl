# encoding: UTF-8

require 'draw_methods'
# Force the drawing to start with a 4byte abgr
class DrawingExtensions
  include Lqpl::Drawing::DrawMethods
  attr_accessor :gc
  attr_accessor :bi
  def initialize
    @bi = BufferedImage.new(500, 500, BufferedImage::TYPE_4BYTE_ABGR)
    @gc = @bi.create_graphics
  end
end
