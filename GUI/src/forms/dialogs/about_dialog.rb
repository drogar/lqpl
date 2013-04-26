java_import javax.swing.JDialog

class AboutDialog < JDialog

  attr_accessor :about_data_label
  attr_accessor :ok_button

  def initialize()
    super
    self.title = "About LQPL"
    self.content_pane = make_content_pane

  end

  def make_content_pane
    Panel.new do |cp|
      cp.layout = BoxLayout.new(cp,BoxLayout::Y_AXIS)
      make_data_panel(cp)
      make_button_panel(cp)
    end
  end

  def make_data_panel(cpanel)
    Panel.new do |p|
      @about_data_label = Label.new("")
      p.add(@about_data_label)
      cpanel.add(p)
    end
  end
  def make_button_panel(cpanel)
    Panel.new do |p|
      @ok_button = Button.new("OK")
      p.add(@ok_button)
      self.root_pane.default_button = @ok_button
      cpanel.add(p)
    end
  end

end