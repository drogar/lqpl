class ComponentQuery < GuiQuery
  # Launch the query in the Event Dispatch Thread (EDT),
  # which is the thread reserved for user interfaces.
  #
  def initialize(component_fixture)
    super()
    @component = component_fixture.target
  end
  
end

class TitleQuery < ComponentQuery
  
  def executeInEDT
    @component.title
  end
end

class VisibleQuery < ComponentQuery
  
  def executeInEDT
    @component.is_visible
  end
end

# Can't just do as ComponentFixture - need to specify each fixture, it seems.

class FrameFixture
  def visible?
    return GuiActionRunner.execute(VisibleQuery.new(self))
  end
end