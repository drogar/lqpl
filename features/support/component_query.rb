class ComponentQuery < GuiQuery
  # Launch the query in the Event Dispatch Thread (EDT),
  # which is the thread reserved for user interfaces.
  #
  def initialize(component)
    super()
    @component = component.target if component.class.to_s =~ /Fixture$/
    @component = component if component.class.to_s =~/Javax|JavaAwt/
  end
  
end

class LabelForQuery < ComponentQuery
  def executeInEDT
    @component.label_for
  end
end

class TitleQuery < ComponentQuery
  
  def executeInEDT
    @component.title
  end
end

class OwnerQuery < ComponentQuery
  
  def executeInEDT
    @component.owner
  end
end


class EnabledQuery < ComponentQuery
  
  def executeInEDT
    @component.is_enabled
  end
end
class SelectedComponentQuery < ComponentQuery
  
  def executeInEDT
    @component.selected_component
  end
end

class SelectedTextQuery < ComponentQuery
  
  def executeInEDT
    @component.selected_text
  end
end

class ViewportQuery < ComponentQuery
  
  def executeInEDT
    @component.viewport
  end
end


class ViewQuery < ComponentQuery
  
  def executeInEDT
    @component.view
  end
end

class VisibleQuery < ComponentQuery
  
  def executeInEDT
    @component.is_visible
  end
end

module EDTQuery
  
  def edt_visible?
    return GuiActionRunner.execute(VisibleQuery.new(self))
  end
  
  def edt_enabled?
    return GuiActionRunner.execute(EnabledQuery.new(self))
  end
    
  def edt_title
    return GuiActionRunner.execute(TitleQuery.new(self))
  end
  
  def edt_label_for
    return GuiActionRunner.execute(LabelForQuery.new(self))
  end
  
  def edt_owner
    return GuiActionRunner.execute(OwnerQuery.new(self))
  end
    
  def edt_selected_text
    return GuiActionRunner.execute(SelectedTextQuery.new(self))
  end
  
  def edt_viewport
    return GuiActionRunner.execute(ViewportQuery.new(self))
  end
  
  def edt_selected_component
    return GuiActionRunner.execute(SelectedComponentQuery.new(self))
  end
  
  def edt_view
    return GuiActionRunner.execute(ViewQuery.new(self))
  end
end
    
class ComponentFixture
  include EDTQuery
end

class Component
  include EDTQuery
end

  