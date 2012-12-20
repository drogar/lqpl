class AboutController < ApplicationController
  set_model 'AboutModel'
  set_view 'AboutView'
  set_close_action :close

  def handleAbout(about_event)
    open
  end

end
