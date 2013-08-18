
class AboutDialog < STDialogWithOK

  attr_accessor :about_data_label

  def initialize()
    super("About LQPL")
    make_data_panel(content_pane)

  end

  def make_data_panel(cpanel)
    Panel.new do |p|
      @about_data_label = Label.new("")
      p.add(@about_data_label)
      cpanel.add(p)
    end
  end

end