require 'import_java'

RSpec.describe ImportJava do
  subject { ImportJava.do_imports(which_files) }
  describe "awt packages" do
    let(:which_files) { { context: self, awt: file } }
    context 'one file, "Point"' do
      let(:file) { 'Point' }
      it 'defines Point in the top level' do
        subject
        expect { Point.new }.not_to raise_exception
      end
    end
    context 'multiple files, "Point", "Rectangle"' do
      let(:file) { ['Point', 'Rectangle'] }
      it 'defines Point in the top level' do
        subject
        expect { Point.new }.not_to raise_exception
      end
      it 'defines Rectangle in the top level' do
        subject
        expect { Rectangle.new }.not_to raise_exception
      end
    end
  end
  describe "swing packages" do
    let(:which_files) { { context: self, swing: file } }
    context 'one file, "SpinnerNumberModel"' do
      let(:file) { 'SpinnerNumberModel' }
      it 'defines SpinnerNumberModel in the top level' do
        subject
        expect { SpinnerNumberModel.new(10, 1, 20, 1) }.not_to raise_exception
      end
    end
  end
  describe "lang packages" do
    let(:which_files) { { context: self, lang: file } }
    context 'one file, "System"' do
      let(:file) { 'System' }
      it 'defines System in the top level' do
        subject
        expect { System.line_separator }.not_to raise_exception
      end
    end
  end
end
