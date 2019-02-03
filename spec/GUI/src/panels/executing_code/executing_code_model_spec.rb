require 'executing_code_model'
require 'specdata/executing_code_data'

describe ExecutingCodeModel do
  describe 'public instance methods' do
    subject { ExecutingCodeModel.new }
    describe 'the_code' do
      it 'should throw an exception with bad input' do
        expect { subject.the_code = 'junk' }.to raise_error JSON::ParserError, /junk/
      end
      it 'should be singleton list when there is 1 instruction and prepend "  0  " to the item' do
        subject.the_code = CMAP_SINGLE
        expect(subject.the_code[:main]).to eq(['  0  EnScope'])
      end
      it 'should make a list of all instructions and prepend the index of the item ' do
        subject.the_code = CMAP_6
        expect(subject.the_code[:main])
          .to eq(['  0  EnScope',
                  '  1  QLoad "@q" 0',
                  '  2  QApply 0 Hadamard "@q"',
                  '  3  QPullup "@q"',
                  '  4  EnScope',
                  '  5  Measure "@q" 14 6 10'])
      end
      it 'should properly justify indexes when the index > 0 ' do
        subject.the_code = CMAP_2x6
        expect(subject.the_code[:main][9, 11]).to eq([
                                                       '  9  QPullup "@q"',
                                                       ' 10  EnScope',
                                                       ' 11  Measure "@q" 14 6 10',
                                                     ])
      end
      it 'should return the created code map when given correct input' do
        subject.the_code = CMAP_2
        expect(subject.the_code).to eq(main: ['  0  EnScope'],
                                       cflip_fcdelbl0: ['  0  EnScope', '  1  QLoad "@q" 0'])
      end
      it 'should return the nil by default' do
        expect(subject.the_code).to be_nil
      end
      it 'should return the value set into the_code_was_updated' do
        subject.the_code_was_updated = false
        expect(subject.the_code_was_updated?).to be false
        subject.the_code_was_updated = true
        expect(subject.the_code_was_updated?).to be true
      end
    end

    describe 'the_code_pointer' do
      it 'should throw an exception with bad input' do
        expect { subject.the_code_pointer = 'junk' }.to raise_error JSON::ParserError, /junk/
      end
      it 'should return the created code map when given correct input when there is code' do
        subject.the_code = CMAP_2
        subject.the_code_pointer = '{"codepointer" : ["main", 0]}'
        expect(subject.the_code_pointer.qpo_method).to eq(:main)
        expect(subject.the_code_pointer.line_number).to eq(0)
      end

      it 'should return the nil by default' do
        expect(subject.the_code_pointer).to be_nil
      end
      it 'should return nil even when given correct input if code has not been created' do
        subject.the_code_pointer = '{"codepointer" : ["main", 2]}'
        expect(subject.the_code_pointer).to be_nil
      end

      it 'should return nil even when given correct input if the key is not in the code' do
        subject.the_code = CMAP_2
        subject.the_code_pointer = '{"codepointer" : ["junk", 2]}'
        expect(subject.the_code_pointer).to be_nil
      end
      it 'should restrict the range of the pointer to the actual number of lines of code' do
        subject.the_code = CMAP_2
        subject.the_code_pointer = '{"codepointer" : ["main", 17]}'
        expect(subject.the_code_pointer.line_number).to eq(0)

        subject.the_code_pointer = '{"codepointer" : ["cflip_fcdelbl0", 0]}'
        expect(subject.the_code_pointer.line_number).to eq(0)

        subject.the_code_pointer = '{"codepointer" : ["cflip_fcdelbl0", 1]}'
        expect(subject.the_code_pointer.line_number).to eq(1)

        subject.the_code_pointer = '{"codepointer" : ["cflip_fcdelbl0", 2]}'
        expect(subject.the_code_pointer.line_number).to eq(1)

        subject.the_code_pointer = '{"codepointer" : ["cflip_fcdelbl0", 3]}'
        expect(subject.the_code_pointer.line_number).to eq(1)
      end
    end
  end
end
