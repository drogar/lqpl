package com.drogar.qface.screens;

import javax.swing.*;
import java.awt.*;

/**
 * Author: Brett Giles
 * Date: 12-06-03
 * Time: 6:43 PM
 * Copyright: 2012 Drogar Industries Ltd.
 * <p/>
 * Description:
 */
public class ExecutingCode extends JFrame{
    private JTabbedPane codeTabbedPane;
    private JPanel panel1;

    public ExecutingCode() throws HeadlessException {
        super("Executing Code");
        setSize(400,600);
        setLocation(30,350);
        setContentPane(panel1);
    }

}
