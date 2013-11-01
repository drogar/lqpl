
class AboutDialog < STDialogWithOK

  attr_accessor :about_data_label

  def initialize()
    super("About LQPL")
    make_data_panel(self.data_pane)

  end

  def make_data_panel(dpanel)
    Panel.new do |p|
      @about_data_label = Label.new("")
      p.add(@about_data_label)
      dpanel.add(p)
    end
  end

end