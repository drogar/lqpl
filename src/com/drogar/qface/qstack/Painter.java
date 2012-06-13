package com.drogar.qface.qstack;

import javax.swing.*;
import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: gilesb
 * Date: 12-05-18
 * Time: 10:48 AM
 * To change this template use File | Settings | File Templates.
 */
public interface Painter {
    public void setModelElement(Object model);
    public void paintModel(Graphics g, Component parent);
    public void paintModelAtPoint(Graphics g, Component parent, Point center);
    public Icon imageOfModel();
}
