package com.drogar.qface.screens;

import javax.swing.*;
import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: gilesb
 * Date: 12-06-08
 * Time: 12:53 PM
 * To change this template use File | Settings | File Templates.
 */
public class ClassicalStack extends JFrame{
    private JPanel panel1;
    private JLabel classicalStack;

    public ClassicalStack() throws HeadlessException {
        super("Classical Stack");
        // setSize(100,300);
        //         setLocation(100,550);
        setBounds(new Rectangle(270,330,150,400));
        setContentPane(panel1);
    }
}
