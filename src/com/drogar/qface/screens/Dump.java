package com.drogar.qface.screens;

import javax.swing.*;
import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: gilesb
 * Date: 12-06-08
 * Time: 12:54 PM
 * To change this template use File | Settings | File Templates.
 */
public class Dump extends JFrame{
    private JPanel panel1;
    private JLabel dump;

    public Dump() throws HeadlessException {
        super("Dump");
        setSize(200,300);
        setLocation(300,350);
        setContentPane(panel1);
    }
}