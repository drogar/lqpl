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
        setSize(400,300);
        //setDefaultCloseOperation(EXIT_ON_CLOSE);

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
        setContentPane(controlPanel);
        setUpSpinner(stepSpinner, 1,1,100000,1);
        setUpSpinner(recursionSpinner, 10,0,100000,1);
        setUpSpinner(treeDepthSpinner, 4,1,100,1);

        controlPanel.setVisible(false);
        this.addWindowListener(new WindowAdapter() {
                    @Override
                    public void windowClosing(WindowEvent windowEvent) {
                        System.exit(0);
                    }
                });

    }

    public void setUpSpinner(JSpinner spinner, int val, int min, int max, int step){
        final SpinnerNumberModel model = new SpinnerNumberModel(val, min, max, step);
        spinner.setModel(model);
    }


    private JButton trimButton;
    private JButton stepButton;
    private JButton goButton;
    private JSpinner stepSpinner;
    private JSpinner recursionSpinner;
    private JSpinner treeDepthSpinner;
    private JPanel spinnerPanel;
    private JPanel buttonPanel;
    private JPanel controlPanel;
    public JLabel message;
    private JMenuItem file_load;
    private JMenuItem file_close;

    private JMenuItem file_compile;


}
