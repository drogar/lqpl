package com.drogar.qface.components;

import com.drogar.qface.qstack.PaintMe;

import javax.swing.*;
import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: gilesb
 * Date: 12-05-15
 * Time: 4:07 PM
 * To change this template use File | Settings | File Templates.
 */
public class QuantumStackPanel extends JPanel{

    Rectangle rect = new Rectangle(0, 0, 100, 50);

    Rectangle area;
    boolean  odd = true;
    PaintMe quantumStack;

    public QuantumStackPanel() {
        super();
        setBackground(Color.white);
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createLineBorder(Color.black));
    }

    public Dimension getPreferredSize() {
        // Figure out what the layout manager needs and
        // then add 100 to the largest of the dimensions
        // in order to enforce a 'round' bullseye
        Dimension layoutSize = super.getPreferredSize();
        int max = Math.max(layoutSize.width,layoutSize.height);
        return new Dimension(max+800,max+600);
    }

    protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g;
        Rectangle bck = new Rectangle(getWidth(), getHeight());
        Color bgcolour = g2.getBackground();
        g2.setColor(bgcolour);
        g2.fill(bck);
        if (null != quantumStack) {
            quantumStack.paintme(g,this);
        } else {

        }
    }

    public PaintMe getQuantumStack() {
        return quantumStack;
    }

    public void setQuantumStack(PaintMe quantumStack) {
        this.quantumStack = quantumStack;
        repaint();
    }

    //    public void paint(Graphics g){
//        update(g);
//    }
//
//    public void update(Graphics g){
//        Graphics2D g2 = (Graphics2D) g;
//        Dimension dim = getSize();
//        int w = (int) dim.getWidth();
//        int h = (int) dim.getHeight();
//        g2.setStroke(new BasicStroke(8.0f));
//
//
//        area = new Rectangle(dim);
//        rect.setLocation(w / 2 - 50, h / 2 - 25);
//
//        if (odd) {
//            odd = false;
//
//            g2.setPaint(Color.green);
//            g2.fillRect(0, 0, w, h);
//
//        // Draws and fills the newly positioned rectangle.
//            g2.setPaint(Color.black);
//            g2.draw(rect);
//        }    else {
//            odd = true;
//
//            g2.setPaint(Color.blue);
//            g2.fillRect(0, 0, w, h);
//
//            // Draws and fills the newly positioned rectangle.
//            g2.setPaint(Color.red);
//            g2.draw(rect);
//        }
//
//    }
}
