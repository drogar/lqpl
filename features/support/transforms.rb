# encoding: UTF-8
Transform(/^be|not be$/) do |s|
  s == 'be'
end

# rubocop:disable Style/SymbolProc
Transform(/^\d+$/) { |c| c.to_i }
# rubocop:enable Style/SymbolProc
