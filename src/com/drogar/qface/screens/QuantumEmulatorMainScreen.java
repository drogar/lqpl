package com.drogar.qface.screens;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * Author: Brett Giles
 * Date: 12-02-17
 * Time: 12:14 PM
 * Copyright: 2012 Drogar Industries Ltd.
 * <p/>
 * Description:
 */
public class QuantumEmulatorMainScreen extends  JFrame{
    public QuantumEmulatorMainScreen() {
        super("Quantum Emulator");
        setSize(400,500);
        setDefaultCloseOperation(EXIT_ON_CLOSE);

        JMenuBar mbar = new JMenuBar();
        JMenu fmenu = new JMenu("File");

        file_load = new JMenuItem("Load");

        file_close = new JMenuItem("Close");

        file_compile = new JMenuItem("Compile");
        fmenu.add(file_load)   ;
        fmenu.add(file_close);
        fmenu.add(file_compile);
        mbar.add(fmenu)   ;
        setJMenuBar(mbar);
        mbar.setVisible(true);
        setContentPane(panel1);
        this.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent windowEvent) {
                System.exit(0);
            }
        });

    }


    private JPanel panel1;
    private JSpinner treeDepthSpinner;
    public JLabel message;
    private JMenuItem file_load;
    private JMenuItem file_close;

    private JMenuItem file_compile;


}
