require 'size_computer'

# calculate the sizes needed to display the tree
class CanvasSize
  attr_accessor :left_width
  attr_accessor :right_width
  attr_accessor :height

  VERTICAL_NODE_SEPARATION = 50.0
  HORIZONTAL_NODE_SEPARATION = 55.0
  def self.new_with_measures(left = 0.0, right = 0.0, height = 0.0)
    cs = new
    cs.initialize_with_measures(left, right, height)
    cs
  end

  def self.new_from_subtree(subtree_array)
    size_computer = SizeComputer.new(subtree_array)
    new_with_measures(size_computer.left_width,
                      size_computer.right_width, size_computer.height)
  end

  def self.compute_offsets(subtree_array)
    SizeComputer.new(subtree_array).compute_offsets
  end

  def initialize_with_measures(left, right, height)
    self.left_width  = left || 0.0
    self.right_width = right || 0.0
    self.height      = height || 0.0
  end

  def total_width
    @left_width + @right_width
  end

  def required_width(side = nil)
    return [total_width, HORIZONTAL_NODE_SEPARATION].max unless side
    return left_required_width if side == :left

    right_required_width
  end

  def left_required_width
    [@left_width, HORIZONTAL_NODE_SEPARATION * 0.5].max
  end

  def right_required_width
    [@right_width, HORIZONTAL_NODE_SEPARATION * 0.5].max
  end

  def height_with_spacing
    @height + VERTICAL_NODE_SEPARATION
  end

  def max_of_each_dimension!(other_canvas_size)
    @left_width = [@left_width, other_canvas_size.left_width].max
    @right_width = [@right_width, other_canvas_size.right_width].max
    @height = [@height, other_canvas_size.height].max
  end

  def self.vertical_node_separation
    node_separation(:vertical)
  end

  def self.node_separation(direction)
    case direction
    when :vertical then VERTICAL_NODE_SEPARATION
    when :horizontal then HORIZONTAL_NODE_SEPARATION
    else VERTICAL_NODE_SEPARATION
    end
  end
end
