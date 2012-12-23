module ComponentSearches

  def spinner_for_label(label_text,frm=$qe_frame)
    theLabel = frm.label(JLabelMatcher.with_text label_text)
    theLabel.should_not == nil
    theLabel.should be_edt_visible
    label_for = theLabel.edt_label_for
    JSpinnerFixture.new($robot,label_for) 
  end
  
  def all_text_is_in_text_component(message_table,text_fixture)
    message_table.hashes.collect {|h| Regexp.new h.values[0]}.each do |t|
      text_fixture.text.should =~ t
    end
  end
  
  def any_text_is_in_text_component(message_table,text_fixture)
    message_texts = message_table.hashes.collect {|h| Regexp.new h.values[0]}
    message_texts.any? do |t|
      text_fixture.text =~ t
    end
  end
  
end

World(ComponentSearches)