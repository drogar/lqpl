class QuantumEmulatorMainForm < STFrame
  attr_accessor :spinner_panel, :button_panel, :step_spinner,
    :recursion_spinner, :recursion_multiplier_spinner,
    :tree_depth_spinner, :messages_text_area, :step_button,
    :go_button, :trim_button
    
  def initialize()
    super("Quantum Emulator")
    self.name = "Quantum Emulator"
    self.content_pane = Panel.new do |cpane|
      cpane.layout = BoxLayout.new(cpane,BoxLayout::Y_AXIS)
      ScrollPane.new do |spane|
        @messages_text_area = JTextArea.new
        @messages_text_area.name = "messagesTextArea"
        @messages_text_area.editable = false
        spane.viewport_view = messages_text_area
        cpane.add(spane)
      end
      Panel.new do |spinpane|
        spinpane.layout = GridLayout.new(4,2)
        @step_spinner = 
          Spinner.spinner_with_label_and_model("Step Size",1, 1, 
                                               100000, 1,spinpane)
        @recursion_spinner = 
          Spinner.spinner_with_label_and_model("Recursion Depth",1, 1, 
                                               100000, 1,spinpane)
        @tree_depth_spinner =
          Spinner.spinner_with_label_and_model("Tree Depth",4, 1, 
                                               100, 1,spinpane)
        @recursion_multiplier_spinner = 
          Spinner.spinner_with_label_and_model("Recursion Multiplier",
                                               10, 1, 
                                               100000, 1,spinpane)
        spinpane.visible = false
        cpane.add(spinpane)
        @spinner_panel = spinpane
      end
      Panel.new do |buttonpanel|
        buttonpanel.layout = GridLayout.new(1,3)
        @trim_button = Button.new("Trim")
        buttonpanel.add(@trim_button)
        @step_button = Button.new("Step")
        buttonpanel.add(@step_button)
        @go_button = Button.new("Go")
        buttonpanel.add(@go_button)
        buttonpanel.visible = false
        cpane.add(buttonpanel)
        @button_panel = buttonpanel
      end
    end
    self.bounds=Rectangle.new(10,10,400,300)
  end
  
  
end