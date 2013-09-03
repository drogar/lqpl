module FixtureMatchers
  def label_matcher_with_pattern(p)
    JLabelMatcher.with_text(Pattern.compile(p))
  end
end

World(FixtureMatchers)