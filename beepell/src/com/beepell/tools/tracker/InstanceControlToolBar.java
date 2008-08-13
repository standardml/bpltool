package com.beepell.tools.tracker;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JToolBar;

import com.beepell.execution.ProcessInstance;
import com.beepell.execution.ProcessInstanceListener;
import com.beepell.execution.ProcessInstanceState;

/**
 * A tracker tool bar to control a process instance.
 * 
 * @author Tim Hallwyl
 */
public class InstanceControlToolBar extends JToolBar implements ActionListener, ProcessInstanceListener {

    private static final long serialVersionUID = 1L;

    private ProcessInstance instance;

    private final JButton step;

    private final JButton stepAll;

    private final JButton run;

    private final JButton terminate;

    private final JButton exit;

    /**
     * Create a tracker tool bar to control a process instance.
     * 
     * @param instance The process instance to control
     */
    public InstanceControlToolBar(ProcessInstance instance) {
        super("Instance Control", JToolBar.HORIZONTAL);

        step = new JButton();
        step.setActionCommand("Step Forward");
        step.setToolTipText("Step Forward");
        step.addActionListener(this);
        step.setIcon(new ImageIcon(InstanceControlToolBar.class.getResource("step.png"), "Step Forward"));
        step.setEnabled(false);
        this.add(step);

        stepAll = new JButton();
        stepAll.setActionCommand("Step All Threads Forward");
        stepAll.setToolTipText("Step All Threads Forward");
        stepAll.addActionListener(this);
        stepAll.setIcon(new ImageIcon(InstanceControlToolBar.class.getResource("step-all.png"), "Step All Threads Forward"));
        stepAll.setEnabled(false);
        this.add(stepAll);

        run = new JButton();
        run.setActionCommand("Run");
        run.setToolTipText("Run");
        run.addActionListener(this);
        run.setIcon(new ImageIcon(InstanceControlToolBar.class.getResource("run.png"), "Run"));
        run.setEnabled(false);
        this.add(run);

        terminate = new JButton();
        terminate.setActionCommand("Terminate");
        terminate.setToolTipText("Terminate");
        terminate.addActionListener(this);
        terminate.setIcon(new ImageIcon(InstanceControlToolBar.class.getResource("terminate.png"), "Terminate"));
        terminate.setEnabled(false);
        this.add(terminate);

        exit = new JButton();
        exit.setActionCommand("Exit");
        exit.setToolTipText("Exit");
        exit.addActionListener(this);
        exit.setIcon(new ImageIcon(InstanceControlToolBar.class.getResource("exit.png"), "Exit"));
        exit.setEnabled(false);
        this.add(exit);

        
        this.setInstacnce(instance);

        /*
        Settings settings = Settings.getInstance();
        if (settings.getSetting("tools.tracker.mode", "step").equals("step")) {
            step.setEnabled(true);
            stepAll.setEnabled(true);
        } else {
            run.doClick();
        }
        */

    }

    /**
     * Set or change the instance controlled by this tool bar.
     * @param instance
     */
    public synchronized void setInstacnce(ProcessInstance instance) {
        if (this.instance != null) {
            this.instance.removeListner(this);
        }

        this.instance = instance;
        
        if (this.instance == null) {
            this.step.setEnabled(false);
            this.stepAll.setEnabled(false);
            this.run.setEnabled(false);
            this.terminate.setEnabled(false);
            this.exit.setEnabled(false);
            return;
        }
        
        this.run.setEnabled(true);
        if (this.instance.isRunningMode()) {
            this.run.setIcon(new ImageIcon(InstanceControlToolBar.class.getResource("pause.png"), "Pause"));
            this.run.setActionCommand("Pause");
            this.run.setToolTipText("Pause");
        } else {
            this.run.setIcon(new ImageIcon(InstanceControlToolBar.class.getResource("run.png"), "Run"));
            this.run.setActionCommand("Run");
            this.run.setToolTipText("Run");
        }

        if (this.instance.getWaitingActivities().size() == 0) {
            this.step.setEnabled(false);
            this.stepAll.setEnabled(false);
        } else {
            this.step.setEnabled(true);
            this.stepAll.setEnabled(true);
        }

        if (this.instance.getState() == ProcessInstanceState.RUNNING || this.instance.getState() == ProcessInstanceState.STARTING) {
            this.terminate.setEnabled(true);
            this.exit.setEnabled(true);
        } else {
            this.terminate.setEnabled(false);
            this.exit.setEnabled(false);
        }

        this.instance.addListener(this);

    }

    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if ("Step Forward".equals(command)) {
            this.step.setEnabled(false);
            this.stepAll.setEnabled(false);
            this.instance.step();
            return;
        }

        if ("Step All Threads Forward".equals(command)) {
            this.step.setEnabled(false);
            this.stepAll.setEnabled(false);
            this.instance.stepAll();
            return;
        }

        if ("Run".equals(command)) {
            this.run.setIcon(new ImageIcon(InstanceControlToolBar.class.getResource("pause.png"), "Pause"));
            this.run.setActionCommand("Pause");
            this.run.setToolTipText("Pause");
            this.step.setEnabled(false);
            this.stepAll.setEnabled(false);
            this.instance.run();
            return;
        }

        if ("Pause".equals(command)) {
            this.run.setIcon(new ImageIcon(InstanceControlToolBar.class.getResource("run.png"), "Run"));
            this.run.setActionCommand("Run");
            this.run.setToolTipText("Run");
            this.instance.pause();
            return;
        }

        if ("Terminate".equals(command)) {
            this.step.setEnabled(false);
            this.stepAll.setEnabled(false);
            this.run.setEnabled(false);
            this.exit.setEnabled(false);
            instance.terminate();
            this.terminate.setEnabled(false);
            return;
        }

        if ("Exit".equals(command)) {
            this.step.setEnabled(false);
            this.stepAll.setEnabled(false);
            this.run.setEnabled(false);
            this.terminate.setEnabled(false);
            instance.exit();
            this.exit.setEnabled(false);
            return;
        }

    }

    public void stateChange(ProcessInstance instance, ProcessInstanceState oldState, ProcessInstanceState newState) {
        if (newState != ProcessInstanceState.RUNNING && newState != ProcessInstanceState.STARTING) {
            this.step.setEnabled(false);
            this.stepAll.setEnabled(false);
            this.run.setEnabled(false);
            this.terminate.setEnabled(false);
            this.exit.setEnabled(false);
        }

    }

    public void stepExpected(ProcessInstance instance) {
        this.step.setEnabled(true);
        this.stepAll.setEnabled(true);
    }

}
