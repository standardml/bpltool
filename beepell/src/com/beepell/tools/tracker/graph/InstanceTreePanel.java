package com.beepell.tools.tracker.graph;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.filechooser.FileSystemView;

import net.sf.epsgraphics.ColorMode;
import net.sf.epsgraphics.EpsGraphics;

import com.beepell.activity.AbstractActivity;
import com.beepell.activity.Activity;
import com.beepell.activity.ActivityState;
import com.beepell.activity.StateChangeListener;
import com.beepell.execution.ProcessInstance;
import com.beepell.execution.ProcessInstanceListener;
import com.beepell.execution.ProcessInstanceState;

/**
 * Graphic panel showing the instance execution tree.
 * 
 * @author Tim Hallwyl
 */
public class InstanceTreePanel extends JPanel implements StateChangeListener, ProcessInstanceListener {

    private static final long serialVersionUID = 1L;

    private final Font font = new Font("FreeSans", Font.BOLD, 14);

    private final Font stateFont = new Font("FreeSans", Font.ITALIC, 10);

    private final Font nameFont = new Font("FreeSans", Font.PLAIN, 10);

    private final BasicStroke stroke = new BasicStroke(4, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);

    private final ProcessInstance instance;

    private TreeNode root;

    private final JLabel processStatusLabel;

    private boolean drawStateLabels = true;

    private boolean useColors = true;

    private Activity focusActivity = null;

    /**
     * Create a graphic panel showing the instance execution tree.
     * 
     * @param instance
     */
    public InstanceTreePanel(ProcessInstance instance) {
        this.instance = instance;
        setBackground(Color.WHITE);

        processStatusLabel = new JLabel("Starting", new ImageIcon(InstanceTreePanel.class.getResource("starting.png"), "Starting"), JLabel.CENTER);
        processStatusLabel.setVerticalTextPosition(JLabel.BOTTOM);
        processStatusLabel.setHorizontalTextPosition(JLabel.CENTER);

        this.setLayout(new FlowLayout(FlowLayout.LEFT, 30, 30));
        this.add(processStatusLabel);

        instance.addActivityStateChangeListener(this);
        instance.addListener(this);
        stateChange(instance, ProcessInstanceState.STARTING, instance.getState());
    }

    private void updateRoot() {
        root = new StructuredNode(instance.getProcessScope());
        int width = root.getWidth();
        int height = root.getHeight();

        root.x = Math.max(getSize().width / 2, (width / 2) + 30);
        root.y = 65;

        this.setMinimumSize(new Dimension(width + 60, height + 60));
        this.setPreferredSize(new Dimension(width + 60, height + 60));
        if (getMinimumSize().height > getSize().height || getMinimumSize().width > getSize().width)
            this.setSize(getMinimumSize());

    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.swing.JComponent#paint(java.awt.Graphics)
     */
    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);

        if (root == null)
            updateRoot();

        root.x = Math.max(getSize().width / 2, root.getWidth() / 2);
        this.paintTree((Graphics2D) g);
    }

    private void paintTree(Graphics2D g2d) {
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_LCD_HRGB);
        g2d.setStroke(stroke);
        g2d.setFont(font);

        paint(root, null, g2d);
    }

    private void paint(final TreeNode node, final TreeNode parent, final Graphics2D g2d) {
        paintNode(node, parent, g2d);

        if (!node.isLeaf()) {
            int x = node.x - (node.getWidth() / 2);
            int y = node.y + 100;

            for (TreeNode child : node.getChildren()) {
                x = x + (child.getWidth() / 2);
                child.x = x;
                child.y = y;
                paint(child, node, g2d);
                x = x + (child.getWidth() / 2) + 30;
            }
        }

    }

    private void paintNode(TreeNode node, TreeNode parent, Graphics2D g2d) {
        String text;
        if (parent == null)
            text = "Process";
        else
            text = node.toString();

        if (focusActivity != null && node.getActivity().equals(focusActivity)) {
            final Point focusPoint = new Point(node.x - 65, node.y - 65);
            focusActivity = null;
            SwingUtilities.invokeLater(new Runnable() {

                public void run() {
                    scrollRectToVisible(new Rectangle(focusPoint, new Dimension(130, 130)));
                }
            });
        }

        Ellipse2D circle = new Ellipse2D.Double(node.x - 35, node.y - 35, 70, 70);

        g2d.setColor(getColor(node.getActivity()));

        if (parent != null) {
            if (node.x == parent.x)
                g2d.drawLine(node.x, node.y - 35, parent.x, parent.y + 35);
            else {
                GeneralPath path = new GeneralPath(GeneralPath.WIND_EVEN_ODD, 4);
                path.moveTo(node.x, node.y - 35);
                path.lineTo(node.x, node.y - 50);
                path.lineTo(parent.x, node.y - 50);
                path.lineTo(parent.x, parent.y + 35);
                g2d.draw(path);
            }
        }

        g2d.draw(circle);

        if (instance.getWaitingActivities().contains(node.getActivity())) {
            if (instance.getWaitingActivities().get(0).equals(node.getActivity()))
                g2d.setColor(READY);
            else
                g2d.setColor(WAITING);

            Ellipse2D c2 = new Ellipse2D.Double(node.x - 40, node.y - 40, 80, 80);
            g2d.draw(c2);
        }

        g2d.setColor(Color.DARK_GRAY);
        g2d.setFont(font);
        FontMetrics metrics = g2d.getFontMetrics(font);
        int x = node.x - (metrics.stringWidth(text) / 2) + 1;
        int y = node.y + (metrics.getAscent() / 2);
        g2d.drawString(text, x, y);

        if (drawStateLabels) {
            g2d.setFont(stateFont);
            text = node.getActivity().getState().toString();
            FontMetrics metrics2 = g2d.getFontMetrics(stateFont);
            int width = metrics2.stringWidth(text);
            int height = metrics2.getAscent();
            g2d.drawString(text, node.x - (width / 2), y + height + 5);
        }

        boolean drawNameLabels = true;
        if (drawNameLabels) {
            text = ((AbstractActivity) node.getActivity()).getName();
            if (text != null) {
                g2d.setFont(nameFont);
                FontMetrics metrics2 = g2d.getFontMetrics(nameFont);
                int width = metrics2.stringWidth(text);
                int height = metrics2.getAscent();
                g2d.drawString(text, node.x - (width / 2), y - height - 5);
            }
        }

    }

    public void addedChild(Activity parent, Activity child) {
        updateRoot();
        repaint();
        focusActivity = child;
    }

    public void stateChanged(Activity activity, ActivityState oldState, ActivityState newState) {
        this.repaint();
    }

    // blue group
    private static final Color RUNNING = new Color(102, 153, 255);

    private static final Color WAITING = new Color(204, 204, 255);

    private static final Color READY = new Color(0, 102, 255);

    private static final Color INITIALIZING = new Color(0, 0, 255);

    // gray
    private static final Color COMPLETED = new Color(204, 204, 204);

    // red
    private static final Color TERMINATING = new Color(255, 51, 51);

    private static final Color TERMINATED = new Color(255, 153, 153);

    // yellow
    private static final Color FAILING = new Color(255, 255, 102);

    private static final Color FAILED = new Color(255, 255, 102);

    private static final Color COMPENSATING = new Color(255, 255, 51);

    private static final Color COMPENSATED = new Color(255, 255, 153);

    private Color getColor(Activity activity) {
        if (!useColors) {
            return Color.BLACK;
        }

        switch (activity.getState()) {
        case RUNNING:
            return RUNNING;
        case WAITING:
            return WAITING;
        case READY:
            return READY;
        case INITIALIZING:
            return INITIALIZING;
        case COMPLETED:
            return COMPLETED;
        case TERMINATING:
            return TERMINATING;
        case TERMINATED:
            return TERMINATED;
        case FAILING:
            return FAILING;
        case FAILED:
            return FAILED;
        case COMPENSATING:
            return COMPENSATING;
        case COMPENSATED:
            return COMPENSATED;

        default:
            return COMPLETED;
        }
    }

    public void stateChange(ProcessInstance instance, ProcessInstanceState oldState, ProcessInstanceState newState) {
        switch (newState) {
        case STARTING:
            processStatusLabel.setIcon(new ImageIcon(InstanceTreePanel.class.getResource("starting.png"), "Starting"));
            processStatusLabel.setText("Starting");
            break;

        case RUNNING:
            processStatusLabel.setIcon(new ImageIcon(InstanceTreePanel.class.getResource("running.png"), "Running"));
            processStatusLabel.setText("Running");
            break;

        case COMPLETED:
            processStatusLabel.setIcon(new ImageIcon(InstanceTreePanel.class.getResource("completed.png"), "Completed"));
            processStatusLabel.setText("Completed");
            break;

        case FAILED:
            processStatusLabel.setIcon(new ImageIcon(InstanceTreePanel.class.getResource("failed.png"), "Failed"));
            processStatusLabel.setText("Failed");
            break;

        case STOPPED:
            processStatusLabel.setIcon(new ImageIcon(InstanceTreePanel.class.getResource("failed.png"), "Stopped"));
            processStatusLabel.setText("Stopped");
            break;

        default:
            break;
        }

    }

    public void stepExpected(ProcessInstance instance) {
    }

    /**
     * Save graph as EPS
     */
    public void saveToFile() {
        try {
            File desktop = new File(System.getProperty("user.home") + File.separator + "Desktop");
            File home = new File(System.getProperty("user.home"));
            File directory;
            if (desktop.exists())
                directory = desktop;
            else
                directory = home;

            JFileChooser fileChooser = new JFileChooser(directory, FileSystemView.getFileSystemView());
            fileChooser.setAcceptAllFileFilterUsed(true);
            fileChooser.setSelectedFile(new File(directory.getCanonicalPath() + File.separator + instance.getProcessScope().getName() + ".eps"));

            int status = fileChooser.showSaveDialog(this);
            if (status == JFileChooser.APPROVE_OPTION) {
                File selectedFile = fileChooser.getSelectedFile();
                if (!selectedFile.getName().endsWith(".eps"))
                    selectedFile = new File(selectedFile.getCanonicalPath() + ".eps");

                PrintStream stream = new PrintStream(selectedFile);
                EpsGraphics g2d = new EpsGraphics("Instance", stream, 0, 0, root.getWidth() + 60, root.getHeight() + 60, useColors ? ColorMode.COLOR_CMYK : ColorMode.BLACK_AND_WHITE);
                root.x = (root.getWidth() / 2) + 30;
                this.paintTree(g2d);
                g2d.close();
                stream.close();
            }
        } catch (IOException exception) {
            JOptionPane.showMessageDialog(this, "Failed to save the image.", "Error", JOptionPane.ERROR_MESSAGE);

        }

    }

    /**
     * @return drawStateLabels
     */
    public boolean drawStateLabels() {
        return drawStateLabels;
    }

    /**
     * @param drawStateLabels
     */
    public void setDrawStateLabels(boolean drawStateLabels) {
        this.drawStateLabels = drawStateLabels;
        this.repaint();
    }

    /**
     * @return the useColors
     */
    public boolean useColors() {
        return useColors;
    }

    /**
     * @param useColors the useColors to set
     */
    public void setUseColors(boolean useColors) {
        this.useColors = useColors;
        this.repaint();
    }

}
