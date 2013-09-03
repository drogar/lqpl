Transform /^be|not be$/ do |s|
  s == 'be'
end

Transform /^\d+$/ do |c|
  c.to_i
end