package com.drogar.lqpl.macosx;

import com.apple.eawt.*;
import com.drogar.lqpl.about.AboutDialog;

import javax.swing.*;

/**
 * Created with IntelliJ IDEA.
 * User: gilesb
 * Date: 12-06-08
 * Time: 11:40 AM
 * To change this template use File | Settings | File Templates.
 */
public class MacOSAboutHandler implements AboutHandler {

        public void handleAbout(AppEvent.AboutEvent aboutEvent){
            new AboutDialog();
        }
}
