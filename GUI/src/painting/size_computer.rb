require 'array_partitioner'

class SizeComputer
  attr_accessor :height

  def initialize(subtree_array)
    subtree_array ||= []
    @partitioned_subtree_array = Lqpl::Utilities::ArrayPartitioner.new(subtree_array)
    @height = subtree_array.map(&:height_with_spacing).max

    initialize_left_middle_and_right
  end

  def left_width
    width = @middle ? @middle.required_width(:left) : 0
    width += @left_size_computer.total_width if @left_size_computer
    width
  end

  def right_width
    width = @middle ? @middle.required_width(:right) : 0
    width += @right_size_computer.total_width if @right_size_computer
    width
  end

  def total_width
    left_width + right_width
  end

  def compute_offsets
    return [] if @partitioned_subtree_array.empty?

    left_offsets + right_offsets
  end

  def left_offsets
    return [] if @partitioned_subtree_array.qpl_left_partition_tails.nil?

    lefts = @partitioned_subtree_array.qpl_left_partition_tails.map do |la|
      -width_to_right_of_head(la)
    end
    left_offsets_detail(lefts)
  end

  def right_offsets
    return [] if @partitioned_subtree_array.qpl_right_partition_heads.nil?

    rights = @partitioned_subtree_array.qpl_right_partition_heads.map do |ra|
      width_to_left_of_tail(ra)
    end
    right_offsets_with_mid(rights)
  end

  private

  def left_offsets_detail(lefts)
    mid = @partitioned_subtree_array.qpl_middle_element

    if mid
      mid_left_width = mid.left_required_width
      lefts.map! { |w| w - mid_left_width }
      lefts << 0
    end
    lefts
  end

  def right_offsets_with_mid(rights)
    mid = @partitioned_subtree_array.qpl_middle_element

    rights.map { |r| r + (mid ? mid.right_required_width : 0) }
  end

  def total_widths(sizes)
    sizes.reduce(0.0) { |acc, elem| acc + elem.required_width }
  end

  def width_to_right_of_head(subarray)
    return 0.0 if subarray.nil? || subarray.empty?

    sub_sizes = subarray.drop(1)
    subarray[0].right_required_width + total_widths(sub_sizes)
  end

  def width_to_left_of_tail(subarray)
    return 0.0 if subarray.nil? || subarray.empty?

    sub_sizes = drop_last(subarray)
    subarray.last.left_required_width + total_widths(sub_sizes)
  end

  def drop_last(subarray)
    subarray.take(subarray.length - 1)
  end

  def initialize_left_middle_and_right
    initialize_middle
    initialize_left_computer
    initialize_right_computer
  end

  def initialize_middle
    @middle = @partitioned_subtree_array.qpl_middle_element
  end

  def initialize_left_computer
    return if @partitioned_subtree_array.left_empty?

    @left_size_computer = SizeComputer.new(@partitioned_subtree_array.qpl_left_partition)
  end

  def initialize_right_computer
    return if @partitioned_subtree_array.right_empty?

    @right_size_computer = SizeComputer.new(@partitioned_subtree_array.qpl_right_partition)
  end
end
