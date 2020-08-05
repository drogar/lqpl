TESTMENU_STRUCTURE = {
  file: { menu_index: 0,
          menu_titles: %w[Load Compile Simulate],
          menu_enabled: [true, true, true] },
  view: { menu_index: 1,
          menu_titles: ['Hide Classical Stack', 'Hide Dump',
                        'Hide Executing Code', 'Hide Stack Translation'],
          menu_enabled: [false, false, false, false] }
}.freeze

# testing class to hold menu bar
class TestMenuParent
  attr_accessor :mbar
  alias make_menu_bar mbar=
end

describe LqplMenu do
  context 'menu initialization' do
    before(:each) do
      @p = TestMenuParent.new
    end
    it 'should successfully create a new menu' do
      SwingRunner.on_edt do
        m = LqplMenu.new(@p)
        expect(m).not_to be_nil
      end
    end
    it 'should set the mbar of the parent' do
      SwingRunner.on_edt do
        LqplMenu.new(@p)
        expect(@p.mbar).not_to be_nil
      end
    end
    it 'should set the mbar of the parent to visible' do
      SwingRunner.on_edt do
        LqplMenu.new(@p)
        expect(@p.mbar.visible).to be true
      end
    end
    it 'should set up two menus (when on mac)' do
      java.lang.System.set_property('apple.laf.useScreenMenuBar', 'true')
      java.lang.System.set_property('com.drogar.testing.fest', 'false')
      # reset so not_on_mac, on_mac work as expected
      SwingRunner.on_edt do
        LqplMenu.new(@p)
        expect(@p.mbar.menu_count).to eq(2)
      end
      java.lang.System.set_property('apple.laf.useScreenMenuBar', 'false')
      java.lang.System.set_property('com.drogar.testing.fest', 'true')
    end
  end
  TESTMENU_STRUCTURE.each do |m, mtestdata|
    context "#{m} menu" do
      before(:each) do
        java.lang.System.set_property('apple.laf.useScreenMenuBar', 'true')
        java.lang.System.set_property('com.drogar.testing.fest', 'false')
        SwingRunner.on_edt do
          @p = TestMenuParent.new
          @lm = LqplMenu.new(@p)
          @tmenu = @p.mbar.getMenu(mtestdata[:menu_index])
        end
      end
      after :each do
        java.lang.System.set_property('apple.laf.useScreenMenuBar', 'false')
        java.lang.System.set_property('com.drogar.testing.fest', 'true')
      end
      it "should have #{mtestdata[:menu_titles].size} menuitems" do
        SwingRunner.on_edt { expect(@tmenu.getItemCount).to eq(mtestdata[:menu_titles].size) }
      end
      it "should have the title #{mtestdata[:menu_titles][1]} for item 1" do
        SwingRunner.on_edt { expect(@tmenu.getItem(1).getText).to eq(mtestdata[:menu_titles][1]) }
      end
      it "should have #{mtestdata[:menu_enabled][1] ? 'enabled' : 'disabled'} menu item 1" do
        SwingRunner.on_edt { expect(@tmenu.getItem(1).isEnabled).to eq(mtestdata[:menu_enabled][1]) }
      end
    end
  end
end
