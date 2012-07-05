module ComponentSearchs
  def check_component_contains_label(container,message_text)
    found = false
    container.components.each do |comp|
      case comp
      when javax.swing.JLabel then
        found |= message_text.any? {|mt| comp.text =~ Regexp.new(mt)}
      when java.awt.Container then
        found |= check_component_contains_label(comp,message_text)
      end
    end
    found
  end
end

World(ComponentSearchs)