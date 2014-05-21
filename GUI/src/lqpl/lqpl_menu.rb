# encoding: utf-8
%w(JMenuBar JMenu JMenuItem).each do |nm|
  java_import 'javax.swing.' + nm
end

# menu handling and loading for the main screen
class LqplMenu
  attr_accessor :view_classical_stack, :view_dump, :view_executing_code, :view_stack_translation

  attr_accessor :mbar, :file_compile, :file_load, :file_simulate

  # next are for win/linux only. Mac is handled in lqpl_controller
  attr_accessor :file_exit, :help_about

  def self.prepare_file_menu_actions(add_listener)
    { 'the_menu.file_compile' => 'file_compile',
      'the_menu.file_load' => 'file_load',
      'the_menu.file_simulate' => 'file_simulate' }.each do |k, v|
        add_listener.call(type: :action, components: { k => v })
      end
  end

  def self.prepare_view_menu_actions(add_listener)
    ['the_menu.view_classical_stack', 'the_menu.view_dump' , 'the_menu.view_executing_code',
     'the_menu.view_stack_translation'].each do |k|
      add_listener.call(type: :action, components: { k => 'view_sub_panel' })
    end
  end

  def self.prepare_up_exit_and_about(add_listener)
    on_mac do
      java_import com.apple.eawt.Application
      Application.application.about_handler = AboutController.instance
      Application.application.quit_handler = ExitHandler.instance
    end

    not_on_mac do
      { 'the_menu.file_exit' => 'file_exit', 'the_menu.help_about' => 'help_about' }.each do |k, v|
        add_listener.call(type: :action, components: { k => v })
      end
    end
  end

  def self.prepare_menu_actions(add_listener)
    LqplMenu.prepare_file_menu_actions(add_listener)
    LqplMenu.prepare_view_menu_actions(add_listener)
    LqplMenu.prepare_up_exit_and_about(add_listener)
  end

  def initialize(parent)
    self.mbar = JMenuBar.new
    init_file_menu mbar
    init_view_menu mbar
    init_help_menu mbar

    parent.make_menu_bar(mbar)
    mbar.visible = true
  end

  def init_file_menu(mbar)
    menu_file = JMenu.new('File')
    @file_load = JMenuItem.new('Load')
    @file_compile =  JMenuItem.new('Compile')
    @file_simulate =  JMenuItem.new('Simulate')

    [@file_load, @file_compile, @file_simulate].each { |fm| menu_file.add(fm) }
    #:nocov:
    handle_not_on_mac_file(menu_file)
    #:nocov:
    mbar.add(menu_file)
  end

  #:nocov:
  def handle_not_on_mac_file(menu_file)
    not_on_mac do
      @file_exit = JMenuItem.new('Exit')
      menu_file.add(@file_exit)
    end
  end
  #:nocov:

  def init_view_menu(mbar)
    menu_view = JMenu.new('View')
    @view_classical_stack =  JMenuItem.new('Hide Classical Stack')
    @view_dump =  JMenuItem.new('Hide Dump')
    @view_executing_code =  JMenuItem.new('Hide Executing Code')
    @view_stack_translation =  JMenuItem.new('Hide Stack Translation')

    [@view_classical_stack, @view_dump, @view_executing_code, @view_stack_translation].each do |vm|
      vm.enabled = false
      menu_view.add(vm)
    end
    mbar.add(menu_view)
  end

    #:nocov:
  def init_help_menu(mbar)
    not_on_mac do
      menu_help = JMenu.new('Help')
      @help_about = JMenuItem.new('About')
      menu_help.add(@help_about)
      mbar.add(menu_help)
    end
  end
    #:nocov:
end
