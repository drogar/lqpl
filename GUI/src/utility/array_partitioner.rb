module Lqpl
  module Utilities
    # class to help partition array to assist with drawing the tree and computing sizes
    class ArrayPartitioner
      attr_accessor :array
      def initialize(array)
        self.array = array
      end

      def partion_into_left_mid_right
        [qpl_left_partition, _qpl_middle_element_as_array, qpl_right_partition]
      end

      def qpl_partition(side)
        return qpl_left_partition if side == :left
        return qpl_right_partition if side == :right

        _qpl_middle_element_as_array
      end

      def qpl_left_partition
        array[0, array_half_length]
      end

      def empty?
        array.empty?
      end

      def left_empty?
        array.length <= 1
      end

      def right_empty?
        array.length <= 1
      end

      def qpl_left_partition_tails
        ArrayPartitioner.new(qpl_left_partition).tails
      end

      def qpl_right_partition
        array[(-array_half_length).ceil, array_half_length]
      end

      def qpl_right_partition_heads
        ArrayPartitioner.new(qpl_right_partition).heads
      end

      def qpl_middle_element
        _qpl_middle_element_as_array[0]
      end

      def tails
        array_indices_as_range.map do |i|
          array[i, array.length - i]
        end
      end

      def heads
        array_indices_as_range.map do |i|
          array[0, i + 1]
        end
      end

      private

      def array_indices_as_range
        Range.new(0, (array.length - 1))
      end

      def array_half_length
        array.length / 2.0
      end

      def size_of_middle
        array_half_length.ceil - array_half_length.floor
      end

      def _qpl_middle_element_as_array
        array[array_half_length, size_of_middle]
      end
    end
  end
end
