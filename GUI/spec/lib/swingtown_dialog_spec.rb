require 'spec/spec_helper'

describe STDialog do
  before (:each) do
    SwingRunner::on_edt do
      @sr = STDialog.new
    end
  end

  it "should not be nil" do
    expect(@sr).not_to be_nil
  end
  
  it "should accept an arg and store it in title" do
    SwingRunner::on_edt do
      dt = STDialog.new("a title")
      expect(dt.title).to eq('a title')
    end
  end
  
  it "should accept a block" do
    SwingRunner::on_edt do
      dt = STDialog.new() do |d|
        d.content_pane.add(Panel.new)
        d.content_pane.add(Panel.new)
      end
      expect(dt.content_pane).to have(2).components
    end
  end
end

describe STDialogWithOK do
  it "should not be nil on creation" do
    SwingRunner::on_edt do
      sr = STDialogWithOK.new
      expect(sr).not_to be_nil
    end
  end
  
  it "should accept an arg and store it in title" do
    SwingRunner::on_edt do
      dt = STDialogWithOK.new("a title")
      expect(dt.title).to eq('a title')
    end
  end
  context "OK Button" do
    describe "basic construction" do
      it "should allow setup of data pane in the init block " do
        SwingRunner::on_edt do
          s = STDialogWithOK.new() do |d|
            expect(d).not_to be_nil
         end
        end
      end
      it "should setup items in the init block and the button pane" do
        SwingRunner::on_edt do
          s = STDialogWithOK.new() do |d|
            d.add(Panel.new)
            d.add(Panel.new)
         end
          expect(s.data_pane).to have(2).components
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
          expect(s.content_pane.components[1]).to eq(s.button_pane)
        end
      end    
    end
    describe "details" do
      before (:each) do
        SwingRunner::on_edt do
          @sr = STDialogWithOK.new("a title")
        end
      end
      specify {expect(@sr.data_pane).not_to be_nil}
      specify {expect(@sr.data_pane.class).to eq(Panel)}
      specify {expect(@sr.button_pane).not_to be_nil}
      specify {expect(@sr.ok_button).not_to be_nil}       

      it "should setup the button pane" do
        expect(last(@sr.content_pane.components)).to eq(@sr.button_pane)
      end
      context "the button pane" do
        before (:each) do
          @bp = last(@sr.content_pane.components)
        end
        specify {expect(@bp.class).to eq(Panel)}
        specify {expect(@bp).to have(1).components}
        specify {expect(@bp.components[0].class).to eq(Button)}
        context "the button" do
          before (:each) do
            @btn = @bp.components[0]
          end
          specify {expect(@sr.root_pane.default_button).to eq(@btn)}
          specify {expect(@btn.text).to eq("OK")}
          specify {expect(@sr.ok_button).to eq(@btn)}
        end
      end
    end
  end
end