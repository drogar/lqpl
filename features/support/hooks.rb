# encoding: UTF-8

Before('@startit') do
  start_up_lqpl
end

Before('@compile') do
  start_up_lqpl
  click_menu_item(%w(File Compile))
end

Before('@load_coin') do
  start_up_lqpl
  click_menu_item(%w(File Load))
  approve_file('GUI/testdata/qplprograms', 'coin.reference.qpo')
end

Before('@load_recurse') do
  start_up_lqpl
  click_menu_item(%w(File Load))
  approve_file('GUI/testdata/qplprograms', 'recurse.qpo')
end

After do
  $qe_frame.close if $qe_frame
  $qe_frame = nil
  $robot.clean_up if $robot
  $robot = nil
end
