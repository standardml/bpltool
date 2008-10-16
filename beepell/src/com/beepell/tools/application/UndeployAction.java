package com.beepell.tools.application;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import com.beepell.deployment.DeploymentManager;
import com.beepell.deployment.ProcessContext;
import com.beepell.execution.ProcessInstance;

/**
 * @author Tim Hallwyl
 * 
 */
public class UndeployAction extends AbstractAction {

    private final Application application;

    public UndeployAction(Application parent) {
        this.application = parent;

        ImageIcon icon = new ImageIcon(UndeployAction.class.getResource("undeploy.png"), "Undeploy Process");

        this.putValue(LARGE_ICON_KEY, icon);
        this.putValue(SMALL_ICON, icon);
        this.putValue(NAME, "Undeploy Process");
        this.putValue(SHORT_DESCRIPTION, "Undeploy process description");
        this.putValue(LONG_DESCRIPTION, "Undeploy the selected process description.");

    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        try {
            Object selection = this.application.getSelection();
            if (selection instanceof ProcessContext) {
              DeploymentManager.undeploy((ProcessContext) selection);
              ServerTreeModel.update(this);
            }
            
            if (selection instanceof ProcessInstance) {
                Thread thread = new Thread() {
                    public synchronized void run() {
                        try {
                            Object selection = application.getSelection();
                            ((ProcessInstance) selection).exit();
                        } catch (Exception exception) {
                            JOptionPane.showMessageDialog(application, exception.getLocalizedMessage(), "Exit Failed", JOptionPane.WARNING_MESSAGE);
                        }
                    }
                };
                thread.start();
            }
                    

        } catch (Exception exception) {
            JOptionPane.showMessageDialog(this.application, exception.getLocalizedMessage(), "Undeployment Failed", JOptionPane.WARNING_MESSAGE);
        }

    }

}
