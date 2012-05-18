package com.drogar.qface.qstack;

import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: gilesb
 * Date: 12-05-18
 * Time: 10:48 AM
 * To change this template use File | Settings | File Templates.
 */
public interface PaintMe {
    public void paintme(Graphics g, Component p);
    public void paintmeAtPoint(Graphics g, Component p, Point center);
}
