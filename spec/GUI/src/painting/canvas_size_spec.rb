require 'canvas_size'
require 'quantum_stack_model'
require 'quantum_stack_painter'

describe CanvasSize do
  describe 'Class Methods' do
    describe 'node_separation' do
      it 'should have a default horizontal node sep of 55.0' do
        expect(CanvasSize.node_separation(:horizontal)).to eq(55.0)
      end

      it 'should have a default vertical node sep of 50.0' do
        expect(CanvasSize.node_separation(:vertical)).to eq(50.0)
      end

      it 'should return vertical node sep of 50.0 for unknown args' do
        expect(CanvasSize.node_separation(:whatever)).to eq(50.0)
      end
    end
    describe 'vertical_node_separation' do
      it 'should return node_separation(:vertical)' do
        expect(CanvasSize.vertical_node_separation)
          .to eq(CanvasSize.node_separation(:vertical))
      end
    end
  end
  describe 'initialization' do
    describe 'explicit measures class init' do
      it 'should accept 0 args and create an item with 0 left,right,height' do
        c = CanvasSize.new_with_measures
        expect(c.left_width).to eq(0.0)
        expect(c.right_width).to eq(0.0)
        expect(c.height).to eq(0.0)
      end

      it 'should accept nil args and create an item with 0 left,right,height' do
        c = CanvasSize.new_with_measures nil, nil, nil
        expect(c.left_width).to eq(0.0)
        expect(c.right_width).to eq(0.0)
        expect(c.height).to eq(0.0)
      end
      it 'should accept 1 number arg and create an item with that as left and 0 right,height' do
        c = CanvasSize.new_with_measures 2.0
        expect(c.left_width).to eq(2.0)
        expect(c.right_width).to eq(0.0)
        expect(c.height).to eq(0.0)
      end
      it 'should accept 2 number args and create with those as left,right and zero height' do
        c = CanvasSize.new_with_measures 2.0, 3.0
        expect(c.left_width).to eq(2.0)
        expect(c.right_width).to eq(3.0)
        expect(c.height).to eq(0.0)
      end
      it 'should accept 3 number args and create an item with those as left,right,height' do
        c = CanvasSize.new_with_measures 1.0, 2.0, 3.0
        expect(c.left_width).to eq(1.0)
        expect(c.right_width).to eq(2.0)
        expect(c.height).to eq(3.0)
      end
    end
  end
  context 'get measures' do
    context 'when sizes > mins' do
      before :each do
        @cs = CanvasSize.new_with_measures(70.0, 80.0, 60.0)
      end
      it 'should give 70.0 as left_width' do
        expect(@cs.left_width).to eq(70.0)
      end
      it 'should give 70.0 as left_required_width' do
        expect(@cs.left_required_width).to eq(70.0)
      end
      it 'should give 150.0 as total width' do
        expect(@cs.total_width).to eq(150.0)
      end
      it 'should give 150.0 as required width' do
        expect(@cs.required_width).to eq(150.0)
      end
      it 'should give 80.0 as right_width' do
        expect(@cs.right_width).to eq(80.0)
      end
      it 'should give 80.0 as right required width' do
        expect(@cs.right_required_width).to eq(80.0)
      end
      it 'should give 60 as height' do
        expect(@cs.height).to eq(60.0)
      end
    end

    context 'when sizes < mins' do
      before :each do
        @cs = CanvasSize.new_with_measures(20.0, 20.0, 20.0)
      end
      it 'should give 20.0 as left_width' do
        expect(@cs.left_width).to eq(20.0)
      end
      it 'should give 1/2 Node Sep as left_required_width' do
        expect(@cs.left_required_width).to eq(CanvasSize::HORIZONTAL_NODE_SEPARATION * 0.5)
      end
      it 'should give 40.0 as total width' do
        expect(@cs.total_width).to eq(40.0)
      end
      it 'should give node sep as required width' do
        expect(@cs.required_width).to eq(CanvasSize::HORIZONTAL_NODE_SEPARATION)
      end
      it 'should give 20.0 as right_width' do
        expect(@cs.right_width).to eq(20.0)
      end
      it 'should give 1/2 node sep as right required width' do
        expect(@cs.right_required_width).to eq(CanvasSize::HORIZONTAL_NODE_SEPARATION * 0.5)
      end
      it 'should give 20+nodesep as height with spacing' do
        expect(@cs.height_with_spacing).to eq(20.0 + CanvasSize::VERTICAL_NODE_SEPARATION)
      end
    end
  end
  context 'self modifies' do
    context 'max of each dimension!' do
      before :each do
        @smaller = CanvasSize.new_with_measures(20.0, 20.0, 20.0)
        @bigger = CanvasSize.new_with_measures(30.0, 30.0, 30.0)
      end
      it 'updates the measures if the other has bigger ones' do
        @smaller.max_of_each_dimension!(@bigger)
        expect(@smaller.left_width).to eq(@bigger.left_width)
        expect(@smaller.right_width).to eq(@bigger.right_width)
        expect(@smaller.height).to eq(@bigger.height)
      end
      it 'stays the same if the other has smaller ones' do
        l = @bigger.left_width
        r = @bigger.right_width
        h = @bigger.height
        @bigger.max_of_each_dimension!(@smaller)

        expect(@bigger.left_width).to eq(l)
        expect(@bigger.right_width).to eq(r)
        expect(@bigger.height).to eq(h)
      end
    end
  end
end
