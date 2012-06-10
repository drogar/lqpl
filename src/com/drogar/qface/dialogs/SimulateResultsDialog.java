package com.drogar.qface.dialogs;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.*;

public class SimulateResultsDialog extends JDialog {
    private JPanel contentPane;
    private JButton buttonOK;
    private JLabel randomValue;
    private JLabel simulateResultsLabel;

    public SimulateResultsDialog() {
        setTitle("Simulate Results");
        setContentPane(contentPane);
        setModal(true);
        getRootPane().setDefaultButton(buttonOK);

        buttonOK.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onOK();
            }
        });

        setBounds(new Rectangle(50,110,200,100));
     //   pack();
      //  setVisible(true);
    }

    private void onOK() {
// add your code here
        dispose();
    }
}
