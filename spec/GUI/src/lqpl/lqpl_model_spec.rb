require 'lqpl_model'

describe LqplModel do
  describe 'toggle_action' do
    it 'should return Hide for Show' do
      expect(LqplModel.toggle_action('Show')).to eq('Hide')
    end
    it 'should return Show for Hide' do
      expect(LqplModel.toggle_action('Hide')).to eq('Show')
    end

    it 'should return Show for anything' do
      expect(LqplModel.toggle_action('whatever')).to eq('Show')
    end
  end
  describe 'new_view_command' do
    it 'should return Hide X Y for input [Show, X, Y]' do
      expect(LqplModel.new_view_command(%w(Show X Y))).to eq('Hide X Y')
    end
    it 'should return Show X Y for input [Hide, X, Y]' do
      expect(LqplModel.new_view_command(%w(Hide X Y))).to eq('Show X Y')
    end
  end
  describe 'symbol_for_view_menu_item' do
    it 'should return :view_menu_x_text= for input [whatever,X]' do
      expect(LqplModel.symbol_for_view_menu_item(%w(whatever X))).to eq(:view_menu_x_text=)
    end
    it 'should return :view_menu_x_y_text= for input [whatever,X, Y]' do
      expect(LqplModel.symbol_for_view_menu_item(%w(whatever X Y))).to eq(:view_menu_x_y_text=)
    end
  end
  describe 'instance methods' do
    subject { LqplModel.new }
    let(:file) { double }
    describe :compile do
      before :each do
        subject.compiler_connection = double
        allow(subject.compiler_connection).to receive(:compile)
        allow(subject.compiler_connection).to receive(:success_or_fail_message)
        allow(file).to receive(:get_absolute_path)
      end
      it 'sends the file path to the compiler' do
        expect(file).to receive(:get_absolute_path).and_return('abc')
        expect(subject.compiler_connection).to receive(:compile).with('abc')
        subject.compile(file)
      end
      it 'sets messages_text to the result of the compile' do
        expect(subject.compiler_connection).to receive(:success_or_fail_message).with(no_args)
          .and_return('abc')
        subject.compile(file)
        expect(subject.messages_text).to eql('abc')
      end
    end
    describe 'enable_view_menu_items' do
      before :each do
        subject.enable_view_menu_items
      end
      it 'sets view_menu_stack_translation_enabled to true' do
        expect(subject.view_menu_stack_translation_enabled).to be true
      end
      it 'sets view_menu_dump_enabled to true' do
        expect(subject.view_menu_dump_enabled).to be true
      end
      it 'sets view_menu_executing_code_enabled to true' do
        expect(subject.view_menu_executing_code_enabled).to be true
      end
      it 'sets view_menu_classical_stack_enabled to true' do
        expect(subject.view_menu_classical_stack_enabled).to be true
      end
    end
    describe 'enable_buttons!' do
      before :each do
        subject.enable_buttons!('test')
      end
      it 'should set the title to Quantum Emulator - <arg>' do
        expect(subject.frame_title).to eq('Quantum Emulator - test')
      end
      it 'should set the go button to enabled' do
        expect(subject.go_enabled).to be true
      end
      it 'should set the step button to enabled' do
        expect(subject.step_enabled).to be true
      end
      it 'should set the spinner panel to visible' do
        expect(subject.spinner_panel_visible).to be true
      end
      it 'should set the button panel to visible' do
        expect(subject.button_panel_visible).to be true
      end
      it 'should set the message to <arg> was loaded.' do
        expect(subject.messages_text).to eq('test was loaded.')
      end
    end
    describe 'toggle_view_menu' do
      it 'should switch hide to show and back' do
        subject.toggle_view_menu(%w(Hide Dump))
        expect(subject.view_menu_dump_text).to eq('Show Dump')
        subject.toggle_view_menu(%w(Show Dump))
        expect(subject.view_menu_dump_text).to eq('Hide Dump')
      end
    end
  end
end
