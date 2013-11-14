class CanvasSize
  attr_accessor :left_width
  attr_accessor :right_width
  attr_accessor :height

  VERTICAL_NODE_SEPARATION=50.0
  HORIZONTAL_NODE_SEPARATION=55.0
  def self.new_with_measures(left=0.0,right=0.0,height=0.0)
    cs=self.new
    cs.initialize_with_measures(left,right,height)
    cs
  end

  def self.new_from_subtree(subtree_array)
    left = CanvasSize::subtree_left_width(subtree_array)
    right = CanvasSize::subtree_right_width(subtree_array)
    h = subtree_array.collect{|cs| cs.height_with_spacing}.max
    self.new_with_measures(left,right,h)
  end

  def self.subtree_left_width(subtree_array)
    self.subtree_side_width(subtree_array, :left)
  end

  def self.subtree_right_width(subtree_array)
    self.subtree_side_width(subtree_array, :right)
  end

  def self.subtree_side_width(subtree_array, side)
    mid = subtree_array.get_middle_element
    CanvasSize::total_widths(subtree_array.get_partition(side))+(mid ? mid.required_width(side) : 0)
  end
  def self.total_widths(sizes)
    sizes.inject(0.0){|memo,s| memo+s.required_width}
  end

  def self.width_to_right_of_head(sizes)
    return 0.0 if !sizes or sizes.length == 0
    sizes[0].right_required_width + CanvasSize::total_widths(sizes.drop(1))
  end

  def self.width_to_left_of_tail(sizes)
    return 0.0 if !sizes or sizes.length == 0
    sizes.last.left_required_width + CanvasSize::total_widths(sizes.take(sizes.length-1))
  end

  # computes offsets for painting the substacks.
  # sum up the sizes from point to midpoint to get offsets (-ve to left, +ve to right)
  # special cases:
  # No sizes -> nothing to return
  # handle having a midpoint = equals size 0
  def self.compute_offsets(sizes)
    return [] if !sizes or sizes.length == 0

    mid = sizes.get_middle_element
    CanvasSize::left_offsets(sizes,mid) + CanvasSize::right_offsets(sizes,mid)
  end

  def self.left_offsets(sizes,mid)
    lefts = sizes.get_left_partition.tails.collect {|la| -CanvasSize.width_to_right_of_head(la)}
    lefts.collect! {|b| b-mid.left_required_width} if mid
    lefts << 0 if mid
    lefts
  end

  def self.right_offsets(sizes,mid)
    rights = sizes.get_right_partition.heads.collect{|ra| CanvasSize.width_to_left_of_tail(ra)}
    rights.collect{|r| r+(mid ? mid.right_required_width : 0)}
  end

  def initialize_with_measures(left,right,height)
    self.left_width = left || 0.0
    self.right_width=right || 0.0
    self.height=height || 0.0
  end

  def total_width
    @left_width+@right_width
  end

  def required_width(side=nil)
    return [total_width, HORIZONTAL_NODE_SEPARATION].max unless side
    return left_required_width if side == :left
    right_required_width
  end

  def left_required_width
    [@left_width, HORIZONTAL_NODE_SEPARATION*0.5].max
  end

  def right_required_width
    [@right_width, HORIZONTAL_NODE_SEPARATION*0.5].max
  end

  def height_with_spacing
    @height + VERTICAL_NODE_SEPARATION
  end

  def max_of_each_dimension!(other_canvas_size)
    @left_width = [@left_width,other_canvas_size.left_width].max
    @right_width = [@right_width,other_canvas_size.right_width].max
    @height = [@height,other_canvas_size.height].max
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