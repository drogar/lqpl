# encoding: UTF-8
# module to help with finding variosu Swing components
module ComponentMethods
  def spinner_for_label(label_text, frm = $qe_frame)
    the_label = frm.label(JLabelMatcher.with_text label_text)
    expect(the_label).not_to be_nil
    expect(the_label).to be_edt_visible
    label_for = the_label.edt_label_for
    JSpinnerFixture.new($robot, label_for)
  end

  def change_spinner_for_label(label_text, count, dir, frm = $qe_frame)
    spinner = spinner_for_label(label_text, frm)
    spinner.increment(count) if dir == 'up'
    spinner.decrement(count) if dir == 'down'
    spinner
  end

  def spinner_for_label_should_be_visible(spinner_label, frm = $qe_frame)
    spinner = spinner_for_label(spinner_label, frm)
    expect(spinner).not_to be_nil
    expect(spinner).to be_edt_visible
    spinner
  end

  def all_text_is_in_text_component(message_table, text_fixture)
    message_table.hashes.map { |h| Regexp.new h.values[0] }.each do |t|
      expect(text_fixture.text).to match(t)
    end
  end

  def any_text_is_in_text_component(message_table, text_fixture)
    message_texts = message_table.hashes.map { |h| Regexp.new h.values[0] }
    message_texts.any? do |t|
      text_fixture.text =~ t
    end
  end
end

World(ComponentMethods)
