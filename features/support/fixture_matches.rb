# matcher for a label in swing
module FixtureMatchers
  def label_matcher_with_pattern(pattern)
    JLabelMatcher.with_text(Pattern.compile(pattern))
  end
end

World(FixtureMatchers)
