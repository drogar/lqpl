# Swing stuff.
module Swingtown
  # I don't know what MiG is all about
  module MiG
    # extend the class methods in the include
    module ClassMethods
      HERE = __dir__

      def mig_jar(glob_path = "#{HERE}/../../java/*.jar")
        warn "mig_jar #{glob_path} "
        Dir.glob(glob_path).find { |f| f =~ /(miglayout-)(.+).jar$/ }
      end

      def mig_layout
        require mig_jar
      end
    end

    def self.included(base)
      base.extend(MiG::ClassMethods)
    end

    def mig_layout(layout_spec)
      Java.net.miginfocom.swing::MigLayout.new layout_spec
    end
  end
  # core swing stuff
  module Core
    # the constants of swing
    module SwingConstants
      %w[
        BOTTOM
        CENTER
        EAST
        HORIZONTAL
        LEADING
        LEFT
        NEXT
        NORTH
        NORTH_EAST
        NORTH_WEST
        PREVIOUS
        RIGHT
        SOUTH
        SOUTH_EAST
        SOUTH_WEST
        TOP
        TRAILING
        VERTICAL
        WEST
      ].each do |konst|
        class_eval "#{konst} = Java::javax::swing::SwingConstants::#{konst}", __FILE__, __LINE__
      end
    end

    # reopen java.awt.Dimension
    class Dimension
      def self.[](width, height)
        java.awt::Dimension.new width, height
      end
    end

    # class ImageIcon
    #    def self.load(image_path)
    #      javax.swing.ImageIcon.new load_resource image_path
    #    end
    #  end

    # A button wrapper
    # See http://xxxxxxxx to understand Swing buttons
    class Button < Java.javax.swing::JButton
      def initialize(*args)
        super(*args)
        yield self if block_given?
      end

      def self.make_button_in_container(container, *args)
        new(*args) { |btn| container.add(btn) }
      end
    end

    # jmenubar wrapper
    class MenuBar < Java.javax.swing.JMenuBar
      def initialize
        super
        yield self if block_given?
      end
    end

    # jmenuitem wrapper
    class MenuItem < Java.javax.swing.JMenuItem
      def initialize
        super
        yield self if block_given?
      end
    end

    # jmenu wrapper
    class Menu < Java.javax.swing.JMenu
      def initialize
        super
        yield self if block_given?
      end
    end

    # Font wrapper
    class SFont < Java.java.awt.Font
    end

    # A label  wrapper
    # See http://xxxxxxxx to understand Swing labels
    class Label < Java.javax.swing::JLabel
      @default_font = SFont.new('Lucida Grande', 0, 12)
      class << self
        attr_accessor :default_font
      end

      def initialize(text = nil)
        super
        self.text = text.to_s if text
        self.font = Label.default_font
        yield self if block_given?
      end

      def minimum_dimensions(width, height)
        self.minimum_size = java.awt::Dimension.new width, height
      end

      def prefered_dimensions(width, height)
        self.prefered_size = java.awt::Dimension.new width, height
      end
    end

    # JSpinner wrapper
    class Spinner < Java.javax.swing.JSpinner
      JINT = Java.int
      SPINNER_CONS = SpinnerNumberModel.java_class.constructor(JINT, JINT, JINT, JINT)
      def self.make_new_spinner_number_model(args)
        SPINNER_CONS.new_instance(args[0], args[1], args[2], args[3])
      end

      def initialize
        super
      end

      def self.make_spinner(*args)
        return Spinner.new unless args && args.length == 4

        spinner = Spinner.new

        spinner.model = make_new_spinner_number_model(args)
        spinner
      end

      def labelize_and_add_to_container(text_for_label, container)
        spinlab = make_my_label(text_for_label)
        container.add(spinlab) if container
        container.add(self) if container
      end

      def self.spinner_with_label(text_for_label, container = nil)
        spinner = make_spinner
        spinner.labelize_and_add_to_container(text_for_label, container)
      end

      def self.spinner_with_label_and_model(text_for_label, model)
        spinner = make_spinner(model[:start], model[:min], model[:max], model[:increment])

        spinner.labelize_and_add_to_container(text_for_label, model[:container])
      end

      def make_my_label(text_for_label)
        spinlab = Label.new(text_for_label)
        spinlab.label_for = self
        spinlab
      end
    end

    # JTextField wrapper
    class TextField < Java.javax.swing.JTextField
      @default_font = SFont.new('Lucida Grande', 0, 12)

      class << self
        attr_accessor :default_font
      end

      def initialize(text = nil)
        super
        self.text = text.to_s
        self.font = Label.default_font

        yield self if block_given?
      end

      def minimum_dimensions(width, height)
        self.minimum_size = java.awt::Dimension.new(width, height)
      end

      def prefered_dimensions(width, height)
        self.preferred_size = java.awt::Dimension.new(width, height)
      end
    end

    # A LayeredPane wrapper
    # See http://xxxx xxxx to understand Swing LayeredPanes
    class LayeredPane < javax.swing.JLayeredPane
      def initialize(*args)
        super
        yield self if block_given?
      end

      def background_color(red, blue, green)
        self.background = java.awt::Color.new(red.to_i, blue.to_i, green.to_i)
      end

      def size(width, height)
        self.preferred_size = java.awt::Dimension.new(width, height)
      end

      def add_ordered_components(*components)
        components.each do |c|
          add c
        end

        components.each do |c|
          moveToFront c
        end
      end
    end

    # JTabbedPane wrapper
    class TabbedPane < javax.swing.JTabbedPane
      def initialize(*args)
        super(*args)
        yield self if block_given?
      end
    end

    # JScrollPane wrapper
    class ScrollPane < javax.swing.JScrollPane
      def initialize(*args)
        super(*args)
        yield self if block_given?
      end

      def background_color(red, blue, green)
        self.background = java.awt::Color.new(red.to_i, blue.to_i, green.to_i)
      end

      def size(width, height)
        self.preferred_size = java.awt::Dimension.new(width, height)
      end
    end

    # JPanel  wrapper
    # See http://xxxxxxxx to understand Swing panels
    class Panel < javax.swing.JPanel
      def initialize
        super()
        yield self if block_given?
      end

      def background_color(red, blue, green)
        self.background = java.awt::Color.new(red.to_i, blue.to_i, green.to_i)
      end

      def size(width, height)
        self.preferred_size = java.awt::Dimension.new(width, height)
      end
    end

    # A frame  wrapper
    # See http://xxxxxxxx to understand Swing frames
    class STFrame < Java.javax.swing::JFrame
      attr_reader :minimum_height, :minimum_width

      def initialize(title, options = {})
        super(title)
        options.each { |k, v| send(k, v) }
        yield self if block_given?
      end

      def define_minimum_size(width, height)
        self.minimum_size = java.awt::Dimension.new(width, height)
      end

      def minimum_height=(height)
        define_minimum_size(@minimum_width.to_i,
                            @minimum_height = height.to_i)
      end

      def minimum_width=(width)
        define_minimum_size(@minimum_width = width.to_i,
                            @minimum_height.to_i)
      end
    end

    # JDialog wrapper
    class STDialog < Java.javax.swing::JDialog
      def initialize(title = nil, do_not_yield = false)
        super()
        self.title = title if title
        yield self if block_given? && !do_not_yield
      end
    end

    # Extend dialog, have an ok button
    class STDialogWithOK < STDialog
      attr_accessor :button_pane
      attr_accessor :ok_button
      attr_accessor :data_pane
      def create_button_ok_pane(parent_pane)
        @button_pane = Panel.new do |bp|
          @ok_button = Button.new('OK') do |b|
            bp.add(b)
            root_pane.default_button = b
          end
          parent_pane.add(bp)
        end
      end

      def initialize(title = nil)
        super(title, false)
        root_pane.content_pane = Panel.new do |cpane|
          cpane.layout = BoxLayout.new(cpane, BoxLayout::Y_AXIS)

          self.data_pane = Panel.new { |dp| cpane.add(dp) }
          create_button_ok_pane(cpane)
        end
        yield data_pane if block_given?
      end
    end
  end
end
