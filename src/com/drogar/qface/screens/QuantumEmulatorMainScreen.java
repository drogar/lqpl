package com.drogar.qface.screens;

import com.apple.eawt.Application;
import com.drogar.qface.macosx.MacOSAboutHandler;

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

        setContentPane(controlPanel);
        setUpSpinner(stepSpinner, 1,1,100000,1);
        setUpSpinner(recursionSpinner, 10,0,100000,1);
        setUpSpinner(treeDepthSpinner, 4,1,100,1);
        setUpMenus();
        controlPanel.setVisible(false);
        Application.getApplication().setAboutHandler(new MacOSAboutHandler());
        Application.getApplication().setQuitHandler(null);

        this.addWindowListener(new WindowAdapter() {
                    @Override
                    public void windowClosing(WindowEvent windowEvent) {
                        System.exit(0);
                    }
                });

    }

    private void setUpMenus(){
        JMenuBar mbar = new JMenuBar();
        JMenu fmenu = new JMenu("File");

        file_load = new JMenuItem("Load");

        file_compile = new JMenuItem("Compile");

        file_simulate = new JMenuItem("Simulate");
        fmenu.add(file_load)   ;
        fmenu.add(file_compile);
        fmenu.add(file_simulate);

        JMenu viewMenu = new JMenu("View");
        viewClassicalStackMI = new JMenuItem("Hide Classical Stack");
        viewDumpMI = new JMenuItem("Hide Dump");
        viewExecutingCodeMI = new JMenuItem("Hide Executing Code");
        viewStackTranslationMI = new JMenuItem("Hide Stack Translation");

        viewClassicalStackMI.setEnabled(false);
        viewDumpMI.setEnabled(false);
        viewExecutingCodeMI.setEnabled(false);
        viewStackTranslationMI.setEnabled(false);

        viewMenu.add(viewClassicalStackMI);
        viewMenu.add(viewDumpMI);
        viewMenu.add(viewExecutingCodeMI);
        viewMenu.add(viewStackTranslationMI);

        mbar.add(fmenu)   ;
        mbar.add(viewMenu);
        setJMenuBar(mbar);
        mbar.setVisible(true);

    }

    public void setUpSpinner(JSpinner spinner, int val, int min, int max, int step){
        final SpinnerNumberModel model = new SpinnerNumberModel(val, min, max, step);
        spinner.setModel(model);
    }

    private JMenuItem viewClassicalStackMI;
    private JMenuItem viewDumpMI;
    private JMenuItem viewExecutingCodeMI;
    private JMenuItem viewStackTranslationMI;

    private JButton trimButton;
    private JButton stepButton;
    private JButton goButton;
    private JSpinner stepSpinner;
    private JLabel stepLabelForSpinner;
    private JSpinner recursionSpinner;
    private JLabel   recursionLabelForSpinner;
    private JSpinner treeDepthSpinner;
    private JLabel   treeDepthLabelForSpinner;

    private JPanel spinnerPanel;
    private JPanel buttonPanel;
    private JPanel controlPanel;
    public JLabel message;

    private JMenuItem file_load;

    private JMenuItem file_compile;
    private JMenuItem file_simulate;


}
