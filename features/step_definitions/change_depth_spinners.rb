
Given /^I type "(\d+)" in the "(.*?)" field$/ do |val,field_name|
  spinner = spinner_for_label field_name
  spinner.enter_text_and_commit val
end

Given /^I click the (up|down) on the "(.*?)" spinner (\d+) times?$/ do |up_down,field_name, count|
  spinner = spinner_for_label field_name
  spinner.increment(count.to_i) if up_down == "up"
  spinner.decrement(count.to_i) if up_down == "down"
end