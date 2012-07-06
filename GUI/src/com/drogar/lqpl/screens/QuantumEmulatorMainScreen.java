package com.drogar.lqpl.screens;

import com.drogar.lqpl.Main;

import com.intellij.uiDesigner.core.GridConstraints;
import com.intellij.uiDesigner.core.GridLayoutManager;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


import java.util.ArrayList;
import org.jruby.Ruby;
import org.jruby.RubyInstanceConfig;
import org.jruby.javasupport.JavaEmbedUtils;

/**
 * Author: Brett Giles
 * Date: 12-02-17
 * Time: 12:14 PM
 * Copyright: 2012 Drogar Industries Ltd.
 * <p/>
 * Description:
 */
public class QuantumEmulatorMainScreen extends JFrame {
    public QuantumEmulatorMainScreen() {
        super("Quantum Emulator");
        frameTitle = "Quantum Emulator";
        setBounds(new Rectangle(10, 10, 400, 300));
        //setDefaultCloseOperation(EXIT_ON_CLOSE);

        setContentPane(controlPanel);
        setUpSpinner(stepSpinner, 1, 1, 100000, 1);
        setUpSpinner(recursionSpinner, 1, 1, 100000, 1);
        setUpSpinner(recursionMultiplierSpinner, 10, 1, 100000, 1);
        setUpSpinner(treeDepthSpinner, 4, 1, 100, 1);

        spinnerPanel.setVisible(false);
        buttonPanel.setVisible(false);

        this.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent windowEvent) {
                Ruby runtime = null;
                try {
                    runtime = Main.getRuntime();
                } catch (Exception e) {
                    RubyInstanceConfig config = new RubyInstanceConfig();
                    runtime = JavaEmbedUtils.initialize(new ArrayList(0), config);
                }

                runtime.evalScriptlet("require 'exit_handler';ExitHandler.instance.close_servers");
                System.exit(0);
            }
        });

    }

    public void setUpSpinner(JSpinner spinner, int val, int min, int max, int step) {
        final SpinnerNumberModel model = new SpinnerNumberModel(val, min, max, step);
        spinner.setModel(model);
    }

    private String frameTitle;

    public void setFrameTitle(String t) {
        System.err.println("Doing setTitle");
        this.setTitle(t);
        frameTitle = t;
    }

    public String getFrameTitle() {
        return frameTitle;
    }

    private JButton trimButton;
    private JButton stepButton;
    private JButton goButton;
    private JSpinner stepSpinner;
    private JLabel stepLabelForSpinner;
    private JSpinner recursionSpinner;
    private JLabel recursionLabelForSpinner;
    private JSpinner treeDepthSpinner;
    private JLabel treeDepthLabelForSpinner;

    private JPanel spinnerPanel;
    private JPanel buttonPanel;
    private JPanel controlPanel;
    private JTextArea messagesTextArea;
    private JPanel messagesPanel;
    private JSpinner recursionMultiplierSpinner;
    private JLabel multiplierLabelForSpinner;
    public JLabel message;



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
        controlPanel = new JPanel();
        controlPanel.setLayout(new GridLayoutManager(3, 1, new Insets(0, 0, 0, 0), -1, -1));
        spinnerPanel = new JPanel();
        spinnerPanel.setLayout(new GridLayoutManager(4, 2, new Insets(0, 0, 0, 0), -1, -1));
        controlPanel.add(spinnerPanel, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
        stepSpinner = new JSpinner();
        spinnerPanel.add(stepSpinner, new GridConstraints(0, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        stepLabelForSpinner = new JLabel();
        stepLabelForSpinner.setText("Step Size");
        spinnerPanel.add(stepLabelForSpinner, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        recursionSpinner = new JSpinner();
        spinnerPanel.add(recursionSpinner, new GridConstraints(1, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        recursionLabelForSpinner = new JLabel();
        recursionLabelForSpinner.setText("Recursion Depth");
        spinnerPanel.add(recursionLabelForSpinner, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        treeDepthSpinner = new JSpinner();
        spinnerPanel.add(treeDepthSpinner, new GridConstraints(3, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        treeDepthLabelForSpinner = new JLabel();
        treeDepthLabelForSpinner.setText("Tree Depth");
        spinnerPanel.add(treeDepthLabelForSpinner, new GridConstraints(3, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        recursionMultiplierSpinner = new JSpinner();
        spinnerPanel.add(recursionMultiplierSpinner, new GridConstraints(2, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        multiplierLabelForSpinner = new JLabel();
        multiplierLabelForSpinner.setText("Recursion Multiplier");
        spinnerPanel.add(multiplierLabelForSpinner, new GridConstraints(2, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        buttonPanel = new JPanel();
        buttonPanel.setLayout(new GridLayoutManager(1, 3, new Insets(0, 0, 0, 0), -1, -1));
        buttonPanel.setEnabled(true);
        controlPanel.add(buttonPanel, new GridConstraints(2, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        trimButton = new JButton();
        trimButton.setText("Trim");
        buttonPanel.add(trimButton, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        stepButton = new JButton();
        stepButton.setText("Step");
        buttonPanel.add(stepButton, new GridConstraints(0, 1, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        goButton = new JButton();
        goButton.setText("Go");
        buttonPanel.add(goButton, new GridConstraints(0, 2, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        messagesPanel = new JPanel();
        messagesPanel.setLayout(new BorderLayout(0, 0));
        controlPanel.add(messagesPanel, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
        final JScrollPane scrollPane1 = new JScrollPane();
        messagesPanel.add(scrollPane1, BorderLayout.CENTER);
        messagesTextArea = new JTextArea();
        messagesTextArea.setEditable(false);
        scrollPane1.setViewportView(messagesTextArea);
        stepLabelForSpinner.setLabelFor(stepSpinner);
        recursionLabelForSpinner.setLabelFor(recursionSpinner);
        treeDepthLabelForSpinner.setLabelFor(treeDepthSpinner);
        multiplierLabelForSpinner.setLabelFor(recursionMultiplierSpinner);
    }

    /**
     * @noinspection ALL
     */
    public JComponent $$$getRootComponent$$$() {
        return controlPanel;
    }
}