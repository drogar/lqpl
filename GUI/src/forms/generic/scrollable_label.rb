class ScrollableLabel < STFrame

  attr_accessor :the_scrolling_label

  def initialize(title, bounds)
    super(title, set_bounds: bounds)
    @the_scrolling_label = Label.new("")
    scrollpane = ScrollPane.new(@the_scrolling_label)
    self.content_pane = scrollpane
  end


end