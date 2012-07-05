package com.drogar.lqpl.components;

import com.drogar.lqpl.qstack.Painter;

import javax.swing.*;
import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: gilesb
 * Date: 12-05-15
 * Time: 4:07 PM
 * To change this template use File | Settings | File Templates.
 */
public class QuantumStackPanel extends JScrollPane{

    Painter quantumStackPainter;
    JLabel qsImageLabel;

    public QuantumStackPanel() {
        super();
        qsImageLabel = new JLabel();
        this.setViewportView(qsImageLabel);
        setBackground(Color.white);
    }

    public Dimension getPreferredSize() {
        // Figure out what the layout manager needs and
        // then add 100 to the largest of the dimensions
        // in order to enforce a 'round' bullseye
        Dimension layoutSize = super.getPreferredSize();
        int max = Math.max(layoutSize.width,layoutSize.height);
        return new Dimension(max+800,max+600);
    }

//    protected void paintComponent(Graphics g) {
//        Graphics2D g2 = (Graphics2D) g;
//        Rectangle bck = new Rectangle(getWidth(), getHeight());
//        Color bgcolour = g2.getBackground();
//        g2.setColor(bgcolour);
//        g2.fill(bck);
//        if (null != quantumStack) {
//            quantumStack.paintme(g,this);
//        } else {
//
//        }
//    }

    public Painter getQuantumStackPainter() {
        return quantumStackPainter;
    }

    public void setQuantumStackPainter(Painter quantumStackP) {
        this.quantumStackPainter = quantumStackP;
        Icon qsImage = quantumStackP.imageOfModel();
        qsImageLabel.setIcon(qsImage);
        repaint();
    }


}
