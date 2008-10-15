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

/**
 * @author Tim Hallwyl
 * 
 */
public class DeployAction extends AbstractAction {

    private final Component component;
    private static File openDirectory  = new File("C:\\Documents and Settings\\tiha\\My Documents\\My Workspace\\beepell-thesis\\src\\com\\beepell\\tools\\launch");

    public DeployAction(Component parent) {
        this.component = parent;
        
        ImageIcon icon = new ImageIcon(DeployAction.class.getResource("deploy.png"), "Deploy Process");

        this.putValue(LARGE_ICON_KEY, icon);
        this.putValue(SMALL_ICON, icon);
        this.putValue(NAME, "Deploy Process");
        this.putValue(SHORT_DESCRIPTION, "Deploy process description");
        this.putValue(LONG_DESCRIPTION, "Load and deploy a BPEL process description from file.");
        
        
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        try {
            final JFileChooser fc = new JFileChooser(DeployAction.openDirectory);
            int returnVal = fc.showOpenDialog(this.component);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                DeployAction.openDirectory = fc.getSelectedFile().getParentFile();
                ProcessContext process = DeploymentManager.deploy(fc.getSelectedFile());
                ServerTreeModel.update(this);
            }
        } catch (Exception exception) {
            JOptionPane.showMessageDialog(this.component, exception.getLocalizedMessage(), "Deployment Failed", JOptionPane.WARNING_MESSAGE);
        }

    }

}
