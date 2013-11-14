require 'spec/spec_helper'

describe STDialog do
  before (:each) do
    SwingRunner::on_edt do
      @sr = STDialog.new
    end
  end

  it "should not be nil" do
    @sr.should_not be_nil
  end
  
  it "should accept an arg and store it in title" do
    SwingRunner::on_edt do
      dt = STDialog.new("a title")
      dt.title.should == 'a title'
    end
  end
  
  it "should accept a block" do
    SwingRunner::on_edt do
      dt = STDialog.new() do |d|
        d.content_pane.add(Panel.new)
        d.content_pane.add(Panel.new)
      end
      dt.content_pane.should have(2).components
    end
  end
end

describe STDialogWithOK do
  it "should not be nil on creation" do
    SwingRunner::on_edt do
      sr = STDialogWithOK.new
      sr.should_not be_nil
    end
  end
  
  it "should accept an arg and store it in title" do
    SwingRunner::on_edt do
      dt = STDialogWithOK.new("a title")
      dt.title.should == 'a title'
    end
  end
  context "OK Button" do
    describe "basic construction" do
      it "should allow setup of data pane in the init block " do
        SwingRunner::on_edt do
          s = STDialogWithOK.new() do |d|
            d.should_not be_nil
         end
        end
      end
      it "should setup items in the init block and the button pane" do
        SwingRunner::on_edt do
          s = STDialogWithOK.new() do |d|
            d.add(Panel.new)
            d.add(Panel.new)
         end
          s.data_pane.should have(2).components
        end
      end
      it "should setup items in the init block before the button pane" do
        SwingRunner::on_edt do
          s = STDialogWithOK.new() do |d|
            p1 = Panel.new
            d.add(p1)
            p2 = Panel.new
            d.add(p2)
          end
         # end
          s.content_pane.components[1].should == s.button_pane
        end
      end    
    end
    describe "details" do
      before (:each) do
        SwingRunner::on_edt do
          @sr = STDialogWithOK.new("a title")
        end
      end
      specify {@sr.data_pane.should_not be_nil}
      specify {@sr.data_pane.class.should == Panel}
      specify {@sr.button_pane.should_not be_nil}
      specify {@sr.ok_button.should_not be_nil}       

      it "should setup the button pane" do
        last(@sr.content_pane.components).should == @sr.button_pane
      end
      context "the button pane" do
        before (:each) do
          @bp = last(@sr.content_pane.components)
        end
        specify {@bp.class.should == Panel}
        specify {@bp.should have(1).components}
        specify {@bp.components[0].class.should == Button}
        context "the button" do
          before (:each) do
            @btn = @bp.components[0]
          end
          specify {@sr.root_pane.default_button.should == @btn}
          specify {@btn.text.should == "OK"}
          specify {@sr.ok_button.should == @btn}
        end
      end
    end
  end
end