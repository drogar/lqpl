describe LqplFileChooser do
  describe 'class method opener' do
    before :each do
      SwingRunner.on_edt do
        @j = LqplFileChooser.opener('a', 'b', 'c')
      end
    end
    it 'should create a JFileChooser' do
      expect(@j).not_to be_nil
      expect(@j).to be_a(JFileChooser)
    end
    it 'should set the title to the first parameter' do
      SwingRunner.on_edt do
        expect(@j.dialog_title).to eq('a')
      end
    end
    it 'should set a file_filter whose description is the second parm' do
      SwingRunner.on_edt do
        expect(@j.file_filter.description).to eq('b')
      end
    end
    it 'should only accept files whose extension equals the third parm' do
      SwingRunner.on_edt do
        expect(@j.file_filter.accept(java.io.File.new('~/junk.c'))).to be true
        expect(@j.file_filter.accept(java.io.File.new('~/junk.jun'))).to be false
      end
    end
  end
  describe 'lqpl_assembled_file_opener' do
    before :each do
      SwingRunner.on_edt do
        @qpo = LqplFileChooser.lqpl_assembled_file_opener
      end
    end
    it 'should have a title of "Load LQPO (Assembly) File"' do
      SwingRunner.on_edt do
        expect(@qpo.dialog_title).to eq('Load LQPO (Assembly) File')
      end
    end
    it 'should set a file_filter whose description is "LQPL assembled file"' do
      SwingRunner.on_edt do
        expect(@qpo.file_filter.description).to eq('LQPL assembled file')
      end
    end
    it 'should only accept files whose extension is qpo' do
      SwingRunner.on_edt do
        expect(@qpo.file_filter.accept(java.io.File.new('~/junk.qpo'))).to be true
        expect(@qpo.file_filter.accept(java.io.File.new('~/junk.jun'))).to be false
      end
    end
  end

  describe 'lqpl_source_file_opener' do
    before :each do
      SwingRunner.on_edt do
        @qpl = LqplFileChooser.lqpl_source_file_opener
      end
    end
    it 'should have a title of "Open LQPL File for Compiling"' do
      SwingRunner.on_edt do
        expect(@qpl.dialog_title).to eq('Open LQPL File for Compiling')
      end
    end
    it 'should set a file_filter whose description is "LQPL source file"' do
      SwingRunner.on_edt do
        expect(@qpl.file_filter.description).to eq('LQPL source file')
      end
    end
    it 'should only accept files whose extension is qpl' do
      SwingRunner.on_edt do
        expect(@qpl.file_filter.accept(java.io.File.new('~/junk.qpl'))).to be true
        expect(@qpl.file_filter.accept(java.io.File.new('~/junk.jun'))).to be false
      end
    end
  end
end
