package com.drogar.lqpl.screens;

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
public class ExecutingCode extends JFrame {
    private JTabbedPane codeTabbedPane;
    private JPanel panel1;

    public ExecutingCode() throws HeadlessException {
        super("Executing Code");
        // setSize(400,600);
        // setLocation(30,350);
        setBounds(new Rectangle(10, 330, 250, 400));

        setContentPane(panel1);
    }

    {
// GUI initializer generated by IntelliJ IDEA GUI Designer
// >>> IMPORTANT!! <<<
// DO NOT EDIT OR ADD ANY CODE HERE!
        $$$setupUI$$$();
    }

    /**
     * Method generated by IntelliJ IDEA GUI Designer
     * >>> IMPORTANT!! <<<
     * DO NOT edit this method OR call it in your code!
     *
     * @noinspection ALL
     */
    private void $$$setupUI$$$() {
        panel1 = new JPanel();
        panel1.setLayout(new BorderLayout(0, 0));
        codeTabbedPane = new JTabbedPane();
        panel1.add(codeTabbedPane, BorderLayout.CENTER);
    }

    /**
     * @noinspection ALL
     */
    public JComponent $$$getRootComponent$$$() {
        return panel1;
    }
}