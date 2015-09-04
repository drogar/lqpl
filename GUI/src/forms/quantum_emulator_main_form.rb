# encoding: utf-8

# Swing frame to display the main form with controls for running the quantum emulator
class QuantumEmulatorMainForm < STFrame
  attr_accessor :spinner_panel, :button_panel, :step_spinner, :recursion_spinner,
                :recursion_multiplier_spinner, :tree_depth_spinner, :messages_text_area,
                :step_button, :go_button, :trim_button

  SPINNERS = { step: { name: 'Step Size', start: 1, increment: 1, max: 100_000, min: 1 },
               recursion: { name: 'Recursion Depth', start: 1, increment: 1, max: 100_000, min: 1 },
               tree_depth: { name: 'Tree Depth', start: 4, increment: 1, max: 100, min: 1 },
               recursion_multiplier: { name: 'Recursion Multiplier', start: 10, increment: 1,
                                       max: 100_000, min: 1 } }.freeze

  def initialize
    super('Quantum Emulator')
    self.name = 'Quantum Emulator'
    self.content_pane = make_content_panel
    self.bounds = Rectangle.new(10, 10, 400, 300)
  end

  def make_content_panel
    Panel.new do |cpane|
      cpane.layout = BoxLayout.new(cpane, BoxLayout::Y_AXIS)
      make_scroll_panel(cpane)
      @spinner_panel = make_spinner_panel(cpane)
      @button_panel = make_button_panel(cpane)
    end
  end

  def make_scroll_panel(cpane)
    ScrollPane.new do |spane|
      @messages_text_area = JTextArea.new
      @messages_text_area.name = 'messagesTextArea'
      @messages_text_area.editable = false
      spane.viewport_view = messages_text_area
      cpane.add(spane)
    end
  end

  def make_spinner_panel(cpane)
    Panel.new do |spinpane|
      spinpane.layout = GridLayout.new(4, 2)
      SPINNERS.each { |k, v| make_a_spinner(k, v, spinpane) }
      spinpane.visible = false
      cpane.add(spinpane)
    end
  end

  def make_a_spinner(key, options, parent)
    model = options
    model[:container] = parent
    send(key.to_s + '_spinner=',
         Spinner.spinner_with_label_and_model(options[:name], model))
  end

  def make_button_panel(cpane)
    Panel.new do |buttonpanel|
      buttonpanel.layout = GridLayout.new(1, 3)
      @trim_button = Button.make_button_in_container(buttonpanel, 'Trim')
      @step_button = Button.make_button_in_container(buttonpanel, 'Step')
      @go_button = Button.make_button_in_container(buttonpanel, 'Go')
      buttonpanel.visible = false
      cpane.add(buttonpanel)
    end
  end
end
