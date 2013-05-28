
class AboutDialog < STDialogWithOK

  attr_accessor :about_data_label

  def initialize()
    super("About LQPL")
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

end