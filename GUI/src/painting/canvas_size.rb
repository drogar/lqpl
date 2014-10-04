# encoding: utf-8

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
    left = CanvasSize.subtree_left_width(subtree_array)
    right = CanvasSize.subtree_right_width(subtree_array)
    h = subtree_array.map(&:height_with_spacing).max
    new_with_measures(left, right, h)
  end

  def self.subtree_left_width(subtree_array)
    subtree_side_width(subtree_array, :left)
  end

  def self.subtree_right_width(subtree_array)
    subtree_side_width(subtree_array, :right)
  end

  def self.subtree_side_width(subtree_array, side)
    ap_subtree_array = ArrayPartitioner.new(subtree_array)
    mid = ap_subtree_array.qpl_middle_element
    CanvasSize.total_widths(ap_subtree_array.qpl_partition(side)) +
      (mid ? mid.required_width(side) : 0)
  end

  def self.total_widths(sizes)
    sizes.reduce(0.0) { |a, e| a + e.required_width }
  end

  def self.width_to_right_of_head(sizes)
    return 0.0 if !sizes || sizes.length == 0
    sizes[0].right_required_width + CanvasSize.total_widths(sizes.drop(1))
  end

  def self.width_to_left_of_tail(sizes)
    return 0.0 if !sizes || sizes.length == 0
    sizes.last.left_required_width + CanvasSize.total_widths(sizes.take(sizes.length - 1))
  end

  # computes offsets for painting the substacks.
  # sum up the sizes from point to midpoint to get offsets (-ve to left, +ve to right)
  # special cases:
  # No sizes -> nothing to return
  # handle having a midpoint = equals size 0
  def self.compute_offsets(sizes)
    return [] if !sizes || sizes.length == 0

    mid = ArrayPartitioner.new(sizes).qpl_middle_element
    CanvasSize.left_offsets(sizes, mid) + CanvasSize.right_offsets(sizes, mid)
  end

  def self.left_offsets(sizes, mid)
    lefts = ArrayPartitioner.new(sizes).qpl_left_partition_tails.map do |la|
      -CanvasSize.width_to_right_of_head(la)
    end
    lefts.map! { |b| b - mid.left_required_width } if mid
    lefts << 0 if mid
    lefts
  end

  def self.right_offsets(sizes, mid)
    rights = ArrayPartitioner.new(sizes).qpl_right_partition_heads.map do |ra|
      CanvasSize.width_to_left_of_tail(ra)
    end
    rights.map { |r| r + (mid ? mid.right_required_width : 0) }
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
