module ComponentFixtureExtensions
  #Class does not get into world? Why?
  class FrameFixture
    def visible?
      return GuiActionRunner.execute(GuiQuery.new {
        def executeInEDT
          self.target.is_visible
        end
      })
    end
  end
end

#World(ComponentFixtureExtensions)