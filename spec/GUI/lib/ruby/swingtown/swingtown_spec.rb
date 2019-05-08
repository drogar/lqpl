describe Swingtown do
  describe Dimensional do
    subject { Dimensional[5, 10] }
    it 'creates a java Dimension with []' do
      expect(subject).to be_a_kind_of(java.awt.Dimension)
    end
    it 'sets the width to the first coordinate' do
      expect(subject.width).to eq(5)
    end
    it 'sets the height to the second coordinate' do
      expect(subject.height).to eq(10)
    end
  end
  # describe Dimension do
  #   pending 'can be created by []' do
  #     d = Dimension[10, 20]
  #     expect(d.width).to eq 10
  #     expect(d.height).to eq 20
  #   end
  # end
  describe Button do
    it 'will accept a block on creation' do
      b = Button.new do |btn|
        btn.text = 'tester'
      end
      expect(b.text).to eq 'tester'
    end
    it 'will make a button and add it to a container' do
      c = double('container')
      allow(c).to receive(:add)
      Button.make_button_in_container(c, 'what')
      expect(c).to have_received(:add).with(instance_of(Button))
    end
  end
  describe MenuBar do
    it 'will accept a block on creation' do
      b = MenuBar.new do |mb|
        mb.minimum_size = Dimension.new(100, 200)
      end
      expect(b.minimum_size.width).to eq 100
    end
  end
  describe MenuItem do
    it 'will accept a block on creation' do
      b = MenuItem.new do |mb|
        mb.minimum_size = Dimension.new(100, 200)
      end
      expect(b.minimum_size.width).to eq 100
    end
  end
  describe Menu do
    it 'will accept a block on creation' do
      b = Menu.new do |mb|
        mb.minimum_size = Dimension.new(100, 200)
      end
      expect(b.minimum_size.width).to eq 100
    end
  end
  describe SFont do
    it 'makes an awt Font' do
      expect(SFont.new('Courier', 0, 10)).to be_a_kind_of(Java.java.awt.Font)
    end
  end
  describe Label do
    subject { Label.new("10") }
    it 'sets a default font of 12 point Lucide Grande on creation' do
      expect(subject.font).to eq(Label.default_font)
    end
    it 'sets text to the argument' do
      expect(subject.text).to eq('10')
    end
    it 'will accept a block on creation' do
      b = Label.new do |mb|
        mb.minimum_size = Dimension.new(100, 200)
      end
      expect(b.minimum_size.width).to eq 100
    end
    describe 'minimum_dimensions' do
      it 'sets the minimum size' do
        subject.minimum_dimensions(10, 30)
        expect(subject.minimum_size.width).to eq 10
        expect(subject.minimum_size.height).to eq 30
      end
    end
    describe 'preferred_dimensions' do
      it 'sets the preferred size' do
        subject.preferred_dimensions(10, 30)
        expect(subject.preferred_size.width).to eq 10
        expect(subject.preferred_size.height).to eq 30
      end
    end
  end
  describe Spinner do
    describe 'class methods' do
      describe :make_spinner do
        subject { Spinner.make_spinner }
        it 'creates a default spinner with no args' do
          expect(subject).to be_a_kind_of(Spinner)
        end
        it 'does not assign values to for the model to a default spinner' do
          expect(subject.model.value).to eq(0)
        end
        it 'Makes a spinner with model when passed 4 numbers' do
          sp = Spinner.make_spinner(2, 1, 100, 4)
          expect(sp.model).not_to be_nil
        end
      end
      describe 'spinner with label' do
        it 'makes a spinner with a label' do
          sp = Spinner.spinner_with_label('rando')
          expect(sp).to be_a_kind_of(Spinner)
          expect(sp.label.text).to eq('rando')
        end
        it 'optionally adds to a container and adds the spinner and label to it' do
          c = double('container')
          allow(c).to receive(:add)
          Spinner.spinner_with_label('rando', c)
          expect(c).to have_received(:add).with(instance_of(Label))
          expect(c).to have_received(:add).with(instance_of(Spinner))
        end
      end
      describe :spinner_with_label_and_model do
        let(:container) { double('c', add: nil) }
        let(:model_hash) { { start: 2, min: 1, max: 100, increment: 4, container: container } }
        subject { Spinner.spinner_with_label_and_model('rando', model_hash) }
        it 'creates a new spinner' do
          expect(subject).to be_an_instance_of(Spinner)
        end
        it 'adds the label to the spinner' do
          expect(subject.label.text).to eq('rando')
        end
        it 'adds the model to the spinner' do
          expect(subject.model.number).to eq(2)
        end
      end
    end
  end
  describe TextField do
    subject { TextField.new("10") }
    it 'sets a default font of 12 point Lucide Grande on creation' do
      expect(subject.font).to eq(TextField.default_font)
    end
    it 'sets text to the argument' do
      expect(subject.text).to eq('10')
    end
    it 'will accept a block on creation' do
      b = TextField.new do |mb|
        mb.minimum_size = Dimension.new(100, 200)
      end
      expect(b.minimum_size.width).to eq 100
    end
    describe 'minimum_dimensions' do
      it 'sets the minimum size' do
        subject.minimum_dimensions(10, 30)
        expect(subject.minimum_size.width).to eq 10
        expect(subject.minimum_size.height).to eq 30
      end
    end
    describe 'preferred_dimensions' do
      it 'sets the preferred size' do
        subject.preferred_dimensions(10, 30)
        expect(subject.preferred_size.width).to eq 10
        expect(subject.preferred_size.height).to eq 30
      end
    end
  end
  describe LayeredPane do
    subject { LayeredPane.new }
    it 'will accept a block on creation' do
      b = LayeredPane.new do |mb|
        mb.minimum_size = Dimension.new(100, 200)
      end
      expect(b.minimum_size.width).to eq 100
    end
    describe 'background_color' do
      it 'sets the background' do
        subject.background_color(10, 30, 200)
        expect(subject.background.red).to eq 10
        expect(subject.background.green).to eq 30
        expect(subject.background.blue).to eq 200
      end
    end
    describe 'size' do
      it 'sets the preferred size' do
        subject.size(10, 30)
        expect(subject.preferred_size.width).to eq 10
        expect(subject.preferred_size.height).to eq 30
      end
    end
    describe :add_ordered_components do
      it 'adds each component and then moves them to the front' do
        allow(subject).to receive(:add)
        allow(subject).to receive(:moveToFront)
        subject.add_ordered_components(:one, :two)
        expect(subject).to have_received(:add).with(:one)
        expect(subject).to have_received(:add).with(:two)
        expect(subject).to have_received(:moveToFront).with(:one)
        expect(subject).to have_received(:moveToFront).with(:two)
      end
    end
  end

  describe TabbedPane do
    it 'will accept a block on creation' do
      b = TabbedPane.new do |mb|
        mb.minimum_size = Dimension.new(100, 200)
      end
      expect(b.minimum_size.width).to eq 100
    end
  end

  describe ScrollPane do
    subject { ScrollPane.new }
    it 'will accept a block on creation' do
      b = ScrollPane.new do |mb|
        mb.minimum_size = Dimension.new(100, 200)
      end
      expect(b.minimum_size.width).to eq 100
    end
    describe 'background_color' do
      it 'sets the background' do
        subject.background_color(10, 30, 200)
        expect(subject.background.red).to eq 10
        expect(subject.background.green).to eq 30
        expect(subject.background.blue).to eq 200
      end
    end
    describe 'size' do
      it 'sets the preferred size' do
        subject.size(10, 30)
        expect(subject.preferred_size.width).to eq 10
        expect(subject.preferred_size.height).to eq 30
      end
    end
  end

  describe Panel do
    subject { Panel.new }
    it 'will accept a block on creation' do
      b = Panel.new do |mb|
        mb.minimum_size = Dimension.new(100, 200)
      end
      expect(b.minimum_size.width).to eq 100
    end
    describe 'background_color' do
      it 'sets the background' do
        subject.background_color(10, 30, 200)
        expect(subject.background.red).to eq 10
        expect(subject.background.green).to eq 30
        expect(subject.background.blue).to eq 200
      end
    end
    describe 'size' do
      it 'sets the preferred size' do
        subject.size(10, 30)
        expect(subject.preferred_size.width).to eq 10
        expect(subject.preferred_size.height).to eq 30
      end
    end
  end

  describe STFrame do
    subject { STFrame.new("10") }
    it 'will accept a block on creation' do
      b = STFrame.new('title') do |mb|
        mb.minimum_size = Dimension.new(100, 200)
      end
      expect(b.minimum_size.width).to eq 100
    end
    it 'accepts a title and options and will set values from them' do
      st = STFrame.new('title', set_bounds: Rectangle.new(430, 10, 600, 640))
      expect(st.bounds.x).to eq(430)
      expect(st.title).to eq('title')
    end
    describe 'minimum_dimensions' do
      it 'sets the minimum size' do
        subject.minimum_dimensions(10, 30)
        expect(subject.minimum_size.width).to eq 10
        expect(subject.minimum_size.height).to eq 30
      end
    end
    describe 'minimum_height=' do
      it 'sets the height and leaves the width alone' do
        subject.minimum_dimensions(20, 40)
        subject.minimum_height = 30
        expect(subject.minimum_size.width).to eq(20)
        expect(subject.minimum_size.height).to eq(30)
      end
    end
    describe 'minimum_width=' do
      it 'sets the width and leaves the height alone' do
        subject.minimum_dimensions(20, 40)
        subject.minimum_width = 30
        expect(subject.minimum_size.width).to eq(30)
        expect(subject.minimum_size.height).to eq(40)
      end
    end
  end
    describe STDialog do
    before :each do
      SwingRunner.on_edt { @sr = STDialog.new }
    end

    it 'is not nil' do
      expect(@sr).not_to be_nil
    end

    it 'should accept an arg and store it in title' do
      SwingRunner.on_edt do
        dt = STDialog.new('a title')
        expect(dt.title).to eq('a title')
      end
    end

    it 'should accept a block' do
      SwingRunner.on_edt do
        dt = STDialog.new do |d|
          d.content_pane.add(Panel.new)
          d.content_pane.add(Panel.new)
        end
        expect(dt.content_pane.components.size).to eql(2)
      end
    end
  end

  describe STDialogWithOK do
    it 'should not be nil on creation' do
      SwingRunner.on_edt do
        sr = STDialogWithOK.new
        expect(sr).not_to be_nil
      end
    end

    it 'should accept an arg and store it in title' do
      SwingRunner.on_edt do
        dt = STDialogWithOK.new('a title')
        expect(dt.title).to eq('a title')
      end
    end
    context 'OK Button' do
      describe 'basic construction' do
        it 'should allow setup of data pane in the init block ' do
          SwingRunner.on_edt do
            STDialogWithOK.new { |d| expect(d).not_to be_nil }
          end
        end
        it 'should setup items in the init block and the button pane' do
          SwingRunner.on_edt do
            s = STDialogWithOK.new do |d|
              d.add(Panel.new)
              d.add(Panel.new)
            end
            expect(s.data_pane.components.size).to eql(2)
          end
        end
        it 'should setup items in the init block before the button pane' do
          SwingRunner.on_edt do
            s = STDialogWithOK.new do |d|
              p1 = Panel.new
              d.add(p1)
              p2 = Panel.new
              d.add(p2)
            end
            expect(s.content_pane.components[1]).to eq(s.button_pane)
          end
        end
      end
      describe 'details' do
        before :each do
          SwingRunner.on_edt do
            @sr = STDialogWithOK.new('a title')
          end
        end
        specify { expect(@sr.data_pane).not_to be_nil }
        specify { expect(@sr.data_pane.class).to eq(Panel) }
        specify { expect(@sr.button_pane).not_to be_nil }
        specify { expect(@sr.ok_button).not_to be_nil }

        it 'should setup the button pane' do
          expect(last(@sr.content_pane.components)).to eq(@sr.button_pane)
        end
        context 'the button pane' do
          before :each do
            @bp = last(@sr.content_pane.components)
          end
          specify { expect(@bp.class).to eq(Panel) }
          specify { expect(@bp.components.size).to eql(1) }
          specify { expect(@bp.components[0].class).to eq(Button) }
          context 'the button' do
            before :each do
              @btn = @bp.components[0]
            end
            specify { expect(@sr.root_pane.default_button).to eq(@btn) }
            specify { expect(@btn.text).to eq('OK') }
            specify { expect(@sr.ok_button).to eq(@btn) }
          end
        end
      end
    end
  end
end
