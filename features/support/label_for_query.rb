class LabelForQuery < GuiQuery
  # Launch the app in the Event Dispatch Thread (EDT),
  # which is the thread reserved for user interfaces.
  # FEST will call this method for us before the test.
  #
  def initialize(component)
    super()
    @comp = component
  end
  
  def executeInEDT
    @comp.label_for
  end
end