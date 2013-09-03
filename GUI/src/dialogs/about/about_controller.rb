class AboutController < ApplicationController
  set_model 'AboutModel'
  set_view 'AboutView'
  set_close_action :dispose

  def handleAbout(about_event)
    open
  end

  def ok_button_action_performed
    dispose
  end
end
