# encoding: utf-8
# TODO: rewrite without monkey business
# class to help partition array to assist with drawing the tree and computing sizes
class ArrayPartitioner
  attr_accessor :array
  def initialize(array)
    self.array = array
  end

  def partion_into_left_mid_right
    [qpl_left_partition , _qpl_middle_element_as_array, qpl_right_partition]
  end

  def qpl_partition(side)
    return qpl_left_partition if side == :left
    return qpl_right_partition if side == :right
    _qpl_middle_element_as_array
  end

  def qpl_left_partition
    array[0, array.length / 2]
  end

  def qpl_left_partition_tails
    ArrayPartitioner.new(qpl_left_partition).tails
  end

  def qpl_right_partition
    array[(-(array.length / 2)).ceil, array.length / 2]
  end

  def qpl_right_partition_heads
    ArrayPartitioner.new(qpl_right_partition).heads
  end

  def qpl_middle_element
    _qpl_middle_element_as_array[0]
  end

  def tails
    Range.new(0, (array.length - 1)).map do |i|
      array[i, array.length - i]
    end
  end

  def heads
    Range.new(0, (array.length - 1)).map do |i|
      array[0, i + 1]
    end
  end

  private

  def _qpl_middle_element_as_array
    half_size = array.length / 2
    array[half_size, -((-array.length / 2).ceil + (half_size.floor))]
  end
end
