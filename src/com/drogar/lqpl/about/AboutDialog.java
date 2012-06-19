package com.drogar.lqpl.about;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class AboutDialog extends JDialog {
    private JPanel contentPane;
    private JButton buttonOK;

    public AboutDialog() {
        setContentPane(contentPane);
        setModal(true);
        getRootPane().setDefaultButton(buttonOK);

        buttonOK.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onOK();
            }
        });
        pack();
        setVisible(true);
    }

    private void onOK() {
// add your code here
        dispose();
    }

//    public static void main(String[] args) {
//        AboutDialog dialog = new AboutDialog();
//        dialog.pack();
//        dialog.setVisible(true);
//        System.exit(0);
//    }
}
