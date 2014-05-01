require 'spec/spec_helper'
require 'GUI/src/painting/canvas_size'
require 'GUI/src/panels/quantum_stack/quantum_stack_model'
require 'GUI/src/panels/quantum_stack/quantum_stack_painter'

describe CanvasSize do
  describe "Class Methods" do
    describe "node_separation" do
      it "should have a default horizontal node sep of 55.0" do
        expect(CanvasSize.node_separation(:horizontal)).to eq(55.0)
      end

      it "should have a default vertical node sep of 50.0" do
        expect(CanvasSize.node_separation(:vertical)).to eq(50.0)
      end
      
      it "should return vertical node sep of 50.0 for unknown args" do
        expect(CanvasSize.node_separation(:whatever)).to eq(50.0)
      end
    end
    describe "vertical_node_separation" do
      it "should return node_separation(:vertical)" do
        expect(CanvasSize.vertical_node_separation).to eq(CanvasSize.node_separation(:vertical))
      end
    end
    describe "total_widths" do
      it "should give 0 for []" do
        expect(CanvasSize::total_widths([])).to eq(0.0)
      end
      it "should give the required size for a single element" do
        c1 = CanvasSize::new_with_measures(10,99,0)
        expect(CanvasSize::total_widths([c1])).to eq(c1.required_width)
      end
      
      it "should give the sume of required width for a more than one element" do
        c1 = CanvasSize::new_with_measures(10,99,0)
        c2 = CanvasSize::new_with_measures(75,85,0)
        expect(CanvasSize::total_widths([c1,c2])).to eq(c1.required_width + c2.required_width)
      end
    end
    describe "width_to_right_of_head" do
      it "should give 0 for an empty list" do
        expect(CanvasSize::width_to_right_of_head([])).to eq(0.0)
      end
      it "should give right measure only for a singleton list" do
        c1 = CanvasSize::new_with_measures(10,99,0)
        expect(CanvasSize::width_to_right_of_head([c1])).to eq(99.0)
      end 
      it "should give right measure of head + total of 2nd for a 2 elt list" do
        c1 = CanvasSize::new_with_measures(10,99,0)
        c2 = CanvasSize::new_with_measures(75,85,0)
        expect(CanvasSize::width_to_right_of_head([c1,c2])).to eq(259.0)
      end 
      it "should give right measure  of head + total of rest for a 4 elt list" do
        c1 = CanvasSize::new_with_measures(10,99,0)
        c2 = CanvasSize::new_with_measures(75,85,0)
        c3 = CanvasSize::new_with_measures(100,100,0)
        c4 = CanvasSize::new_with_measures(200,200,0)
        expect(CanvasSize::width_to_right_of_head([c1,c2,c3,c4])).to eq(859.0)
      end        
    end
    
    describe "width_to_left_of_tail" do
      it "should give 0 for an empty list" do
        expect(CanvasSize::width_to_left_of_tail([])).to eq(0.0)
      end
      it "should give left measure only for a singleton list" do
        c1 = CanvasSize::new_with_measures(70,99,0)
        expect(CanvasSize::width_to_left_of_tail([c1])).to eq(70.0)
      end 
      it "should give left measure of last + total of 1st for a 2 elt list" do
        c1 = CanvasSize::new_with_measures(50,99,0)
        c2 = CanvasSize::new_with_measures(75,85,0)
        expect(CanvasSize::width_to_left_of_tail([c1,c2])).to eq(224.0)
      end 
      it "should give left measure  of last + total of rest for a 4 elt list" do
        c1 = CanvasSize::new_with_measures(60,99,0)
        c2 = CanvasSize::new_with_measures(75,85,0)
        c3 = CanvasSize::new_with_measures(100,100,0)
        c4 = CanvasSize::new_with_measures(200,200,0)
        expect(CanvasSize::width_to_left_of_tail([c1,c2,c3,c4])).to eq(60+99+75+85+100+100+200)
      end        
    end
    describe "compute_offsets" do
      it "should have a single 0 offest for a single measure" do
        c1 = CanvasSize::new_with_measures(10,99,0)
        expect(CanvasSize::compute_offsets([c1])).to eq([0])
      end
      describe "left_offsets" do
        it "should be [-99] for a 2 elt list" do
          c1 = CanvasSize::new_with_measures(10,99,0)
          c2 = CanvasSize::new_with_measures(75,85,0)
          sizes = [c1,c2]
          mid = sizes.qpl_middle_element
          expect(CanvasSize::left_offsets(sizes,mid)).to eq([-99])
        end
      end
      describe "right_offsets" do
        it "should be [75] for a 2 elt list" do
          c1 = CanvasSize::new_with_measures(10,99,0)
          c2 = CanvasSize::new_with_measures(75,85,0)
          sizes = [c1,c2]
          mid = sizes.qpl_middle_element
          expect(CanvasSize::right_offsets(sizes,mid)).to eq([75])
        end
      end
      it "should be [-right,left] for a 2 elt list" do
        c1 = CanvasSize::new_with_measures(10,99,0)
        c2 = CanvasSize::new_with_measures(75,85,0)
        expect(CanvasSize::compute_offsets([c1,c2])).to eq([-99,75])
      end
      it "should be [-(1.r+2.l), 0, (2.r+3.l)] for 3 element" do
        c1 = CanvasSize::new_with_measures(10,99,0)
        c2 = CanvasSize::new_with_measures(75,85,0)
        c3 = CanvasSize::new_with_measures(100,100,0)
        expect(CanvasSize::compute_offsets([c1,c2,c3])).to eq([-174,0,185])
      end
      it "should be [-(1.r+2.t), -2.r, 3.l, (3.t+4.l)] for 4 element" do
        c1 = CanvasSize::new_with_measures(10,99,0)
        c2 = CanvasSize::new_with_measures(75,85,0)
        c3 = CanvasSize::new_with_measures(100,100,0)
        c4 = CanvasSize::new_with_measures(200,200,0)
        expect(CanvasSize::compute_offsets([c1,c2,c3,c4])).to eq([-(99+75+85),-85,100,200+200])
      end
      it "should be [-(1.r+2.t+3.l), -(2.r+3.l),0, (3.r), 3.t+4.l, 3.t+4.t+5.l] for 5 element" do
        c1 = CanvasSize::new_with_measures(10,99,0)
        c2 = CanvasSize::new_with_measures(75,85,0)
        c3 = CanvasSize::new_with_measures(80,120,0)
        c4 = CanvasSize::new_with_measures(200,200,0)
        c5 = CanvasSize::new_with_measures(60,60,0)
        expect(CanvasSize::compute_offsets([c1,c2,c3,c4,c5])).to eq([-(99+75+85+80), -(85+80),0,120+200,120+200+200+60])
      end
      it "should return an empty list for empty or nil input" do
        expect(CanvasSize::compute_offsets([])).to eq([])
        expect(CanvasSize::compute_offsets(nil)).to eq([])
      end
    end
  end
  describe "initialization" do
    describe "explicit measures class init" do
      it "should accept 0 args and create an item with 0 left,right,height" do
        c = CanvasSize.new_with_measures
        expect(c.left_width).to eq(0.0)
        expect(c.right_width).to eq(0.0)
        expect(c.height).to eq(0.0)
      end
      
      it "should accept nil args and create an item with 0 left,right,height" do
        c = CanvasSize.new_with_measures nil,nil,nil
        expect(c.left_width).to eq(0.0)
        expect(c.right_width).to eq(0.0)
        expect(c.height).to eq(0.0)
      end
      it "should accept 1 number arg and create an item with that as left and 0 right,height" do
        c = CanvasSize.new_with_measures 2.0
        expect(c.left_width).to eq(2.0)
        expect(c.right_width).to eq(0.0)
        expect(c.height).to eq(0.0)
      end
      it "should accept 2 number args and create an item with those as left,right and zero height" do
        c = CanvasSize.new_with_measures 2.0, 3.0
        expect(c.left_width).to eq(2.0)
        expect(c.right_width).to eq(3.0)
        expect(c.height).to eq(0.0)
      end
      it "should accept 3 number args and create an item with those as left,right,height" do
        c = CanvasSize.new_with_measures 1.0, 2.0, 3.0
        expect(c.left_width).to eq(1.0)
        expect(c.right_width).to eq(2.0)
        expect(c.height).to eq(3.0)
      end
    end
    it "should accept an array of ducktyped CanvasSizes, partition them and use them as left, right, height" do
      c1 = CanvasSize.new_with_measures 100.0, 100.0, 10.0
      c2 = CanvasSize.new_with_measures 45.0, 80.0, 20.0
      c3 = CanvasSize.new_with_measures 70.0, 60.0, 20.0
      c = CanvasSize.new_from_subtree [c1,c2,c3]
      expect(c.left_width).to eq(245.0)
      expect(c.right_width).to eq(210.0)
      expect(c.height).to eq(20.0 + CanvasSize::VERTICAL_NODE_SEPARATION)
    end
  end   
  context "get measures" do
    context "when sizes > mins" do
      before (:each) do
        @cs = CanvasSize.new_with_measures(70.0,80.0,60.0)
      end
      it "should give 70.0 as left_width" do
        expect(@cs.left_width).to eq(70.0)
      end
      it "should give 70.0 as left_required_width" do
        expect(@cs.left_required_width).to eq(70.0)
      end
      it "should give 150.0 as total width" do
        expect(@cs.total_width).to eq(150.0)
      end
      it "should give 150.0 as required width" do
        expect(@cs.required_width).to eq(150.0)
      end
      it "should give 80.0 as right_width" do
        expect(@cs.right_width).to eq(80.0)
      end
      it "should give 80.0 as right required width" do
        expect(@cs.right_required_width).to eq(80.0)
      end
      it "should give 60 as height" do
        expect(@cs.height).to eq(60.0)
      end
    end
    
    context "when sizes < mins" do
      before (:each) do
        @cs = CanvasSize.new_with_measures(20.0,20.0,20.0)
      end
      it "should give 20.0 as left_width" do
        expect(@cs.left_width).to eq(20.0)
      end
      it "should give 1/2 Node Sep as left_required_width" do
        expect(@cs.left_required_width).to eq(CanvasSize::HORIZONTAL_NODE_SEPARATION * 0.5)
      end
      it "should give 40.0 as total width" do
        expect(@cs.total_width).to eq(40.0)
      end
      it "should give node sep as required width" do
        expect(@cs.required_width).to eq(CanvasSize::HORIZONTAL_NODE_SEPARATION)
      end
      it "should give 20.0 as right_width" do
        expect(@cs.right_width).to eq(20.0)
      end
      it "should give 1/2 node sep as right required width" do
        expect(@cs.right_required_width).to eq(CanvasSize::HORIZONTAL_NODE_SEPARATION * 0.5)
      end
      it "should give 20+nodesep as height with spacing" do
        expect(@cs.height_with_spacing).to eq(20.0+CanvasSize::VERTICAL_NODE_SEPARATION)
      end
    end
  end
  context "self modifies" do
    context "max of each dimension!" do
      before(:each) do
        @smaller = CanvasSize.new_with_measures(20.0,20.0,20.0)
        @bigger = CanvasSize.new_with_measures(30.0,30.0,30.0)
      end
      it "updates the measures if the other has bigger ones" do
        @smaller.max_of_each_dimension!(@bigger)
        expect(@smaller.left_width).to eq(@bigger.left_width)
        expect(@smaller.right_width).to eq(@bigger.right_width)
        expect(@smaller.height).to eq(@bigger.height)
      end
      it "stays the same if the other has smaller ones" do
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