# encoding: utf-8
# TODO: rewrite without monkey business
# class to help partition array to assist with drawing the tree and computing sizes
class Array
  def partion_into_left_mid_right
    [qpl_left_partition , _qpl_middle_element_as_array, qpl_right_partition]
  end

  def qpl_partition(side)
    return qpl_left_partition if side == :left
    return qpl_right_partition if side == :right
    _qpl_middle_element_as_array
  end

  def qpl_left_partition
    self[0, size / 2]
  end

  def qpl_right_partition
    self[(-(size / 2)).ceil, size / 2]
  end

  def qpl_middle_element
    _qpl_middle_element_as_array[0]
  end

  def tails
    Range.new(0, (size - 1)).map do |i|
      self[i, size - i]
    end
  end

  def heads
    Range.new(0, (size - 1)).map do |i|
      self[0, i + 1]
    end
  end

  private

  def _qpl_middle_element_as_array
    half_size = size / 2
    self[half_size, -((-size / 2).ceil + (half_size.floor))]
  end
end
