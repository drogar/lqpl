%w{JMenuBar JMenu JMenuItem}.each do |nm|
  java_import  "javax.swing."+nm
end

class LqplMenu
  attr_accessor :view_classical_stack
  attr_accessor :view_dump
  attr_accessor :view_executing_code
  attr_accessor :view_stack_translation

  attr_accessor :file_compile
  attr_accessor :file_load
  attr_accessor :file_simulate

  #next are for win/linux only. Mac is handled in lqpl_controller
  attr_accessor :file_exit
  attr_accessor :help_about

  def initialize(parent)
    mbar = JMenuBar.new
    init_file_menu mbar
    init_view_menu mbar
    init_help_menu mbar

    parent.set_menu_bar(mbar);
    mbar.visible = true;
  end
  
  def init_file_menu(mbar)
    menu_file = JMenu.new("File")
    @file_load = JMenuItem.new("Load");
    @file_compile =  JMenuItem.new("Compile");
    @file_simulate =  JMenuItem.new("Simulate");

    menu_file.add(@file_load);
    menu_file.add(@file_compile);
    menu_file.add(@file_simulate);
    #:nocov:
    not_on_mac do
      @file_exit = JMenuItem.new("Exit")
      menu_file.add(@file_exit)
    end
    #:nocov:
    mbar.add(menu_file)
  end

  def init_view_menu(mbar)
    menu_view = JMenu.new("View");
    @view_classical_stack =  JMenuItem.new("Hide Classical Stack");
    @view_dump =  JMenuItem.new("Hide Dump");
    @view_executing_code =  JMenuItem.new("Hide Executing Code");
    @view_stack_translation =  JMenuItem.new("Hide Stack Translation");

    @view_classical_stack.enabled = false;
    @view_dump.enabled = false;
    @view_executing_code.enabled = false;
    @view_stack_translation.enabled = false;

    menu_view.add(@view_classical_stack);
    menu_view.add(@view_dump);
    menu_view.add(@view_executing_code);
    menu_view.add(@view_stack_translation);
    mbar.add(menu_view)
  end
  
    #:nocov:
  def init_help_menu mbar
    not_on_mac do
      menu_help = JMenu.new("Help")
      @help_about = JMenuItem.new("About")
      menu_help.add(@help_about)
      mbar.add(menu_help)
    end
  end
    #:nocov:

end