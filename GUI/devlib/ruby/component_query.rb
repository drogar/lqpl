# query the value of a swing component
class ComponentQuery < GuiQuery
  # Launch the query in the Event Dispatch Thread (EDT),
  # which is the thread reserved for user interfaces.
  #
  def initialize(component)
    # if a fixture, component is component's target. Otherwise, it is the component.
    super()
    @component = component.target if component.class.to_s =~ /Fixture$/
    @component = component if component.class.ancestors.any? { |c| c.to_s =~ /Javax|JavaAwt/ }
  end

  def self.execute_query(component)
    GuiActionRunner.execute(new(component))
  end
end

# creates a class for the query - name is derived from the method name, but we
# really don't care what it is, we only need to have a class defined so
# that we can use GuiActionRunner on it.
# the class is defined in the context of the toplevel binding, unless it
# already exists.

def query_class_template(class_name, method_call_name)
  %(
     class #{class_name} < ComponentQuery
       def executeInEDT
         @component.#{method_call_name}
       end
     end)
end

def define_edt_query_class(name)
  class_name = "#{name.to_s.chomp('?')}_query".camelize
  begin
    class_name.constatize
  rescue NameError
    method_call_name = name.to_s.chomp('?')
    method_call_name = 'is_' + method_call_name if method_call_name.size < name.to_s.size
    # rubocop:disable Eval
    eval query_class_template(class_name, method_call_name), TOPLEVEL_BINDING
    # rubocop:enable Eval
  end
end

def define_edt_query(name)
  define_edt_query_class name
  define_method("edt_#{name}") do
    return "#{name.to_s.chomp('?')}_query".camelize.constantize.execute_query(self)
  end
end

# various query methods for any swing class element
module EDTQuery
  define_edt_query :visible?
  define_edt_query :enabled?
  define_edt_query :title
  define_edt_query :label_for
  define_edt_query :owner
  define_edt_query :selected_text
  define_edt_query :viewport
  define_edt_query :selected_component
  define_edt_query :view
end

# basic fixture class with edt queries
class ComponentFixture
  include EDTQuery
end

# basic swing class with edt queries
class Component
  include EDTQuery
end
