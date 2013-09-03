
Before('@startit')do
  start_up_lqpl
end

Before('@compile')do
  start_up_lqpl
  click_menu_item(['File', 'Compile'])
end

Before('@load_coin')do
  start_up_lqpl
  click_menu_item(['File', 'Load'])
  approve_file('GUI/testdata/qplprograms','coin.reference.qpo')
end

Before('@load_recurse')do
  start_up_lqpl
  click_menu_item(['File', 'Load'])
  approve_file('GUI/testdata/qplprograms','recurse.qpo')
end

After do
  $qe_frame.close
  $qe_frame = nil
  $robot.clean_up
  $robot = nil
end


