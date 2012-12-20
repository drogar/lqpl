module ComponentSearches
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
  def spinner_for_label(label_text,frm=$qe_frame)
    theLabel = frm.label(JLabelMatcher.with_text label_text)
    theLabel.should_not == nil
    theLabel.should be_edt_visible
    label_for = theLabel.edt_label_for
    JSpinnerFixture.new($robot,label_for) 
  end
end

World(ComponentSearches)