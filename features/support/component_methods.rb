module ComponentMethods

  def spinner_for_label(label_text,frm=$qe_frame)
    theLabel = frm.label(JLabelMatcher.with_text label_text)
    expect(theLabel).not_to be_nil
    expect(theLabel).to be_edt_visible
    label_for = theLabel.edt_label_for
    JSpinnerFixture.new($robot,label_for)
  end

  def change_spinner_for_label(label_text,count,dir,frm=$qe_frame)
    spinner = spinner_for_label(label_text,frm)
    spinner.increment(count) if dir == "up"
    spinner.decrement(count) if dir == "down"
    spinner
  end

  def spinner_for_label_should_be_visible(spinner_label,frm=$qe_frame)
    spinner = spinner_for_label(spinner_label,frm)
    expect(spinner).not_to be_nil
    expect(spinner).to be_edt_visible
    spinner
  end

  def all_text_is_in_text_component(message_table,text_fixture)
    message_table.hashes.collect {|h| Regexp.new h.values[0]}.each do |t|
      expect(text_fixture.text).to match(t)
    end
  end

  def any_text_is_in_text_component(message_table,text_fixture)
    message_texts = message_table.hashes.collect {|h| Regexp.new h.values[0]}
    message_texts.any? do |t|
      text_fixture.text =~ t
    end
  end

end

World(ComponentMethods)