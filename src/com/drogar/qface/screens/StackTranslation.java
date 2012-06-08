package com.drogar.qface.screens;

import javax.swing.*;
import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: gilesb
 * Date: 12-06-08
 * Time: 12:55 PM
 * To change this template use File | Settings | File Templates.
 */
public class StackTranslation extends JFrame{
    private JLabel stackTranslation;
    private JPanel panel1;

    public StackTranslation() throws HeadlessException {
        super("Stack Translation");
        setSize(100,200);
        setLocation(600,650);
        setContentPane(panel1);
    }
}
