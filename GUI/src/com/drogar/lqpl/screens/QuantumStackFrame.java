package com.drogar.lqpl.screens;


import com.drogar.lqpl.components.QuantumStackPanel;

import javax.swing.*;
import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: gilesb
 * Date: 12-05-15
 * Time: 4:04 PM
 * To change this template use File | Settings | File Templates.
 */
public class QuantumStackFrame extends JFrame{
     private QuantumStackPanel quantumStackPanel;

     public QuantumStackFrame() throws HeadlessException {
        super("Quantum Stack");
        quantumStackPanel = new QuantumStackPanel();

        setBounds(new Rectangle(430,10,400,300));
        setContentPane(quantumStackPanel);
        pack();
     }
}
