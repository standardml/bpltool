package com.beepell.ui.dom.graph;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Path2D;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.beepell.ui.dom.graph.conf.Configuration;
import com.beepell.ui.dom.graph.conf.NullConfiguration;

/**
 * @author Tim Hallwyl
 * 
 */
public class DocumentGraph extends JPanel {

    private static final long serialVersionUID = 1L;

    private static final QName stateAttribute = new QName("http://www.itu.dk/research/pls/bpl", "state", "bpl");

    private final Configuration configuration;

    private final Map<QName, ImageIcon> elementIcons;

    private final Map<String, ImageIcon> stateEmblems;

    private final List<QName> borderElements;

    /**
     * It may help the user to turn on/off some states; for example to turn off
     * "completed" to see only "active" and future elements. This is the list of
     * states that must be hidden. We look for the state in the state attribute
     * defined above.
     */
    private final List<String> hiddenStates = new ArrayList<String>();

    /**
     * In the general case we allow to visualize only a sub-tree of a document,
     * rooted at some element. This may be the document element or any other
     * element in the DOM tree. Only the rootElement and its descendant are
     * shown.
     */
    private Element rootElement;

    /**
     * The root node of the internal representation, used to calculate the
     * drawing positions of the tree nodes.
     */
    private TreeNode root;

    private final Font font = new Font("FreeSans", Font.BOLD, 14);

    private final Font stateFont = new Font("FreeSans", Font.ITALIC, 10);

    private final Font nameFont = new Font("FreeSans", Font.PLAIN, 10);

    private final BasicStroke stroke = new BasicStroke(4, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);

    private final BasicStroke borderStroke = new BasicStroke(1, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);

    private boolean drawStateLabels = true;

    private boolean drawColors = true;

    private boolean drawEmblems = true;

    private boolean drawIcons = true;

    private boolean drawBorders = true;
    
    private boolean drawElementNames = true;

    private int scopeLevel = 0;

    /**
     * If an Element is set as the focusElement, then the panel will make sure
     * that it is visible by scrolling the panel.
     */
    private Element focusElement = null;

    /**
     * Create a DOM Graph with visualization configuration.
     * 
     * @param rootElement the element to use as root element in the graph
     * @param configuration the visualization configuration
     */
    public DocumentGraph(Element rootElement, Configuration configuration) {
        super();
        this.setBackground(Color.WHITE);
        this.rootElement = rootElement;
        this.configuration = configuration;
        this.elementIcons = configuration.getElementIcons();
        this.stateEmblems = configuration.getStateEmblems();
        this.borderElements = configuration.getBorderElements();
    }

    /**
     * Create a DOM Graph Panel with showing all elements in the tree, without
     * state label coloring, icons and emblems. This is the same as using the
     * NullConfiguration in the constructor accepting a configuration.
     * 
     * @see DocumentGraph#DocumentGraph(Element, Configuration)
     * @param rootElement
     */
    public DocumentGraph(Element rootElement) {
        this(rootElement, NullConfiguration.getInstance());
    }

    /**
     * Hides elements with the given bpl:state attribute value. Attempt to hide
     * already hidden states have no effect.
     * 
     * @param state the state to hide
     */
    public void hide(String state) {
        if (this.hiddenStates.contains(state))
            return;
        this.hiddenStates.add(state);
        this.repaint();
    }

    /**
     * Shows elements with the given bpl:state attribute value. All states are
     * visible by default, so only hidden states needs to be set visible with
     * this method.
     * 
     * @param state
     */
    public void show(String state) {
        this.hiddenStates.remove(state);
        this.repaint();
    }

    /**
     * @return the useEmblems
     */
    public boolean isDrawEmblems() {
        return this.drawEmblems;
    }

    /**
     * @param useEmblems the useEmblems to set
     */
    public void setDrawEmblems(boolean useEmblems) {
        this.drawEmblems = useEmblems;
        this.repaint();
    }

    /**
     * @return the useIcons
     */
    public boolean isDrawIcons() {
        return this.drawIcons;
    }

    /**
     * @param useIcons the useIcons to set
     */
    public void setDrawIcons(boolean useIcons) {
        this.drawIcons = useIcons;
        this.repaint();
    }

    /**
     * Returns true if the state is not hidden, otherwise false.
     * 
     * @param state the state to check
     * @return true if the state is not hidden
     */
    public boolean isVisible(String state) {
        return !this.hiddenStates.contains(state);
    }

    /**
     * Updating the root will recalculate the complete tree. This does not
     * repaint the panel.
     */
    private void updateRoot() {
        this.root = new StructuredNode(this.rootElement, this.configuration.getVisibleElements());
        int width = this.root.getWidth();
        int height = this.root.getHeight();

        this.root.x = Math.max(getSize().width / 2, (width / 2) + 30);
        this.root.y = 65;

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

        if (this.root == null)
            updateRoot();

        this.root.x = Math.max(getSize().width / 2, this.root.getWidth() / 2);
        this.paintTree((Graphics2D) g);
    }

    private void paintTree(Graphics2D g2d) {
        this.scopeLevel = 0;
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_LCD_HRGB);
        g2d.setStroke(this.stroke);
        g2d.setFont(this.font);

        paint(this.root, null, g2d);
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

    @SuppressWarnings("null")
    private void paintNode(TreeNode node, TreeNode parent, Graphics2D g2d) {
        QName qname = new QName(node.getElement().getNamespaceURI(), node.getElement().getLocalName());

        if (this.hiddenStates.contains(node.getElement().getAttributeNS(stateAttribute.getNamespaceURI(), stateAttribute.getLocalPart())))
            return;

        if (this.drawBorders && this.borderElements != null && this.borderElements.contains(qname)) {
            Rectangle border = new Rectangle(node.x - (node.getWidth() / 2), node.y, node.getWidth(), node.getHeight());
            g2d.setStroke(this.borderStroke);
            if (this.scopeLevel % 2 == 0)
                g2d.setColor(new Color(197, 219, 246));
            else
                g2d.setColor(Color.WHITE);
            g2d.fill(border);
            g2d.setColor(Color.GRAY);
            g2d.draw(border);
            g2d.setStroke(this.stroke);
            this.scopeLevel++;
        }

        // Change color by state label scheme:
        String state = node.getElement().getAttributeNS(stateAttribute.getNamespaceURI(), stateAttribute.getLocalPart());
        Color stateColor = this.configuration.getStateColors() == null ? null : this.configuration.getStateColors().get(state);
        if (stateColor != null && this.drawColors == true)
            g2d.setColor(stateColor);
        else
            g2d.setColor(Color.BLACK);

        // Draw the lines connecting this node to its parent.
        if (parent != null) {
            if (node.x == parent.x)
                g2d.drawLine(node.x, node.y - 35, parent.x, parent.y + 35);
            else {
                GeneralPath path = new GeneralPath(Path2D.WIND_EVEN_ODD, 4);
                path.moveTo(node.x, node.y - 35);
                path.lineTo(node.x, node.y - 50);
                path.lineTo(parent.x, node.y - 50);
                path.lineTo(parent.x, parent.y + 35);
                g2d.draw(path);
            }
        }

        // Draw element icon or draw the circle representing the Element node.
        if (this.drawIcons && this.elementIcons != null && this.elementIcons.containsKey(qname)) {
            ImageIcon icon = this.elementIcons.get(qname);
            int x = node.x - (icon.getIconWidth() / 2) + 1;
            int y = node.y - (icon.getIconHeight() / 2) + 1;
            g2d.drawImage(icon.getImage(), x, y, null);
        } else {
            Ellipse2D circle = new Ellipse2D.Double(node.x - 35, node.y - 35, 70, 70);
            g2d.draw(circle);
        }

        // Draw state emblems
        if (this.drawEmblems && this.stateEmblems != null && state != null && this.stateEmblems.containsKey(state)) {
            ImageIcon icon = this.stateEmblems.get(state);
            int x = node.x + 15 - (icon.getIconWidth() / 2) + 1;
            int y = node.y + 15 - (icon.getIconHeight() / 2) + 1;
            g2d.drawImage(icon.getImage(), x, y, null);
        }

        String text;

        // Draw node text (Element.getLocalName)
        text = node.toString();
        g2d.setFont(this.font);
        FontMetrics metrics = g2d.getFontMetrics(this.font);
        int x = node.x - (metrics.stringWidth(text) / 2) + 1;
        int y = node.y + (metrics.getAscent() / 2);

        // Add a white shadow when icons are used
        if (this.drawIcons && this.drawElementNames) {
            g2d.setColor(Color.WHITE);
            g2d.drawString(node.toString(), x + 1, y + 1);
        }

        g2d.setColor(Color.BLACK);
        if (this.drawElementNames) {
            g2d.drawString(node.toString(), x, y);
        }

        // Draw the state label
        if (this.drawStateLabels && !state.isEmpty()) {
            g2d.setFont(this.stateFont);
            FontMetrics metrics2 = g2d.getFontMetrics(this.stateFont);
            int width = metrics2.stringWidth(state);
            int height = metrics2.getAscent();
            g2d.drawString(state, node.x - (width / 2), y + height + 5);
        }

        boolean drawNameLabels = true;
        text = node.getElement().getAttribute("name");
        if (drawNameLabels && !text.isEmpty()) {
            g2d.setFont(this.nameFont);
            FontMetrics metrics2 = g2d.getFontMetrics(this.nameFont);
            int width = metrics2.stringWidth(text);
            int height = metrics2.getAscent();
            g2d.drawString(text, node.x - (width / 2), y - height - 5);
        }

        /*
         * If the node we are about to paint is the focusElement, we scroll to
         * make sure it is visible.
         */
        if (this.focusElement != null && node.getElement().equals(this.focusElement)) {
            final Point focusPoint = new Point(node.x - 65, node.y - 65);
            this.focusElement = null;
            SwingUtilities.invokeLater(new Runnable() {

                public void run() {
                    scrollRectToVisible(new Rectangle(focusPoint, new Dimension(130, 130)));
                }
            });
        }
    }

    /**
     * 
     * @return the useColors
     */
    public boolean isDrawColors() {
        return this.drawColors;
    }

    /**
     * @param useColors the useColors to set
     */
    public void setDrawColors(boolean useColors) {
        this.drawColors = useColors;
        this.repaint();
    }

    /**
     * @return drawStateLabels
     */
    public boolean isStateLabelsVisible() {
        return this.drawStateLabels;
    }

    /**
     * @param drawStateLabels
     */
    public void setStateLabelsVisible(boolean drawStateLabels) {
        this.drawStateLabels = drawStateLabels;
        this.repaint();
    }

    /**
     * Set (change) the Element node used as root in the displayed graph.
     * 
     * @param root the new Element root node
     */
    public void setRootElement(Element root) {
        this.rootElement = root;
        this.updateRoot();
        this.repaint();
    }

    /**
     * @return the drawBorders
     */
    public boolean isDrawBorders() {
        return this.drawBorders;
    }

    /**
     * @param drawBorders the drawBorders to set
     */
    public void setDrawBorders(boolean drawBorders) {
        this.drawBorders = drawBorders;
        this.repaint();
    }

    /**
     * Gets the configuration used by this document graph.
     * 
     * @return the configuration
     */
    public Configuration getConfiguration() {
        return this.configuration;
    }

    /**
     * @return the drawElementNames
     */
    public boolean isDrawElementNames() {
        return this.drawElementNames;
    }

    /**
     * @param drawElementNames the drawElementNames to set
     */
    public void setDrawElementNames(boolean drawElementNames) {
        this.drawElementNames = drawElementNames;
        this.repaint();
    }

}
