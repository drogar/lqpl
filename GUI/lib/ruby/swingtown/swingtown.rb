module Swingtown
  module MiG
    module ClassMethods
      HERE = File.expand_path(File.dirname __FILE__ )

      def mig_jar glob_path = "#{HERE}/../../java/*.jar"
        warn "mig_jar #{glob_path} "
        Dir.glob(glob_path).select { |f| 
        f =~ /(miglayout-)(.+).jar$/}.first
      end

      def mig_layout
        require mig_jar  
      end

    end

    def self.included(base)
      base.extend(MiG::ClassMethods)
    end

    def mig_layout(layout_spec)
     Java::net::miginfocom::swing::MigLayout.new layout_spec
    end

  end

  module Core
    module SwingConstants
     %w{
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
      WEST}.each do  |konst|
        class_eval "#{konst} = Java::javax::swing::SwingConstants::#{konst}"
      end
    end

    class Dimension
      def self.[](width, height)
        java::awt::Dimension.new width, height
      end
    end

    # class ImageIcon
    #    def self.load(image_path) 
    #      javax.swing.ImageIcon.new load_resource image_path
    #    end
    #  end


    # A button wrapper
    # See http://xxxxxxxx to understand Swing buttons
    class Button < Java::javax::swing::JButton
      def initialize(*args)
        super(*args)
        yield self if block_given?
      end
    end


    class MenuBar < Java::javax.swing.JMenuBar
      def initialize
        super
        yield self if block_given?
      end

    end

    class MenuItem  < Java::javax.swing.JMenuItem
      def initialize
        super
        yield self if block_given?
      end
    end

    class Menu  < Java::javax.swing.JMenu
      def initialize
        super
        yield self if block_given?
      end
    end

    class Font < Java::java.awt.Font
    end

    # A label  wrapper
    # See http://xxxxxxxx to understand Swing labels
    class Label < Java::javax::swing::JLabel

      @@default_font = java::awt.Font.new "Lucida Grande", 0, 12

      def self.default_font=(default_font)
        @@default_font = default_font
      end

      def self.default_font
        @@default_font 
      end

      def initialize(text=nil)
        super
        self.text = text.to_s if text
        self.font = Label.default_font
        yield self if block_given?
      end

      def minimum_dimensions(width, height)
        self.minimum_size = java::awt::Dimension.new width, height
      end

      def prefered_dimensions(width, height)
        self.prefered_size = java::awt::Dimension.new width, height
      end
    end

    class Spinner < Java::javax.swing.JSpinner
      def initialize
        super
      end
      
      def self.spinner_with_label(text_for_label, container=nil)
        spinner = Spinner.new
        spinlab = spinner.make_my_label(text_for_label)
        container.add(spinlab) if container
        container.add(spinner) if container
      end
      def self.spinner_with_label_and_model(text_for_label,val, min, max, step,container=nil)
        spinner = Spinner.new
        spinner.model=SpinnerNumberModel.new(val,min,max,step)
        
        spinlab = spinner.make_my_label(text_for_label)
        container.add(spinlab) if container
        container.add(spinner) if container
      end
      def make_my_label(text_for_label)
        spinlab = Label.new(text_for_label)
        spinlab.label_for = self
        spinlab
      end
    end
    
    class TextField < Java::javax.swing.JTextField

      @@default_font = java::awt.Font.new "Lucida Grande", 0, 12

      def self.default_font=(default_font)
        @@default_font = default_font
      end

      def self.default_font
        @@default_font 
      end

      def initialize(text = nil)
        super
        self.text = text.to_s
        self.font = Label.default_font

        yield self if block_given?
      end

      def minimum_dimensions(width, height)
        self.minimum_size = java::awt::Dimension.new(width, 
                                                     height)
      end

      def prefered_dimensions(width, height)
        self.preferred_size =  java::awt::Dimension.new(width, 
                                                        height)
      end

    end

    # A LayeredPane wrapper
    # See http://xxxx xxxx to understand Swing LayeredPanes 
    class LayeredPane < javax::swing.JLayeredPane

      def initialize(*args)
        super
        yield self if block_given?
      end
      
      def background_color(red, blue, green)
        self.background = java::awt::Color.new(red.to_i, blue.to_i, 
                                               green.to_i)
      end

      def size(width, height)
        self.preferred_size =  java::awt::Dimension.new(width, 
                                                        height)
      end


      def add_ordered_components(*components)
        components.each do |c|
          self.add c
        end

        components.each do |c|
          self.moveToFront c
        end


      end
    end
    
    class TabbedPane < javax::swing.JTabbedPane
      def initialize(*args) 
        super(*args)
        yield self if block_given?
      end
    end

    class ScrollPane < javax::swing.JScrollPane

      def initialize(*args) 
        super(*args)
        yield self if block_given?
      end
      
      def background_color(red, blue, green)
        self.background = java::awt::Color.new(red.to_i, blue.to_i, 
                                               green.to_i)
      end

      def size(width, height)
        self.preferred_size =  java::awt::Dimension.new(width, 
                                                        height)
      end


    end
    # A panel  wrapper
    # See http://xxxxxxxx to understand Swing panels
    class Panel < javax::swing.JPanel

      def initialize()
        super()
        yield self if block_given?
      end
      
      def background_color(red, blue, green)
        self.background = java::awt::Color.new(red.to_i, blue.to_i, 
                                               green.to_i)
      end

      def size(width, height)
        self.preferred_size =  java::awt::Dimension.new(width, 
                                                        height)
      end
    end


    # A frame  wrapper
    # See http://xxxxxxxx to understand Swing frames
    class STFrame  < Java::javax::swing::JFrame
      attr_accessor :minimum_height, :minimum_width
      
      def initialize(title)
        super(title)
        yield self if block_given?
      end
      
      def initialize(*args)
        super
        
      end

      def define_minimum_size(width, height)
        self.minimum_size = java::awt::Dimension.new(width, height)
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
    
    class STDialog < Java::javax::swing::JDialog
      
      def initialize(title=nil)
        super()
        self.title = title if title
        yield self if block_given?
      end
    end
    
    class STDialogWithOK < STDialog
      attr_accessor :button_pane
      attr_accessor :ok_button
      def initialize(title=nil)
        super
        @button_pane = Panel.new do |bp|
          @ok_button = Button.new("OK") do |b|
            bp.add(b)
            self.root_pane.default_button = b
          end
          self.content_pane.add(bp)
        end
      end
    end
  end
end


