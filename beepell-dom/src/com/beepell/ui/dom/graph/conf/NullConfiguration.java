package com.beepell.ui.dom.graph.conf;

import java.awt.Color;
import java.util.List;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.xml.namespace.QName;


/**
 * @author Tim Hallwyl
 * 
 */
public class NullConfiguration implements Configuration {

    private static Configuration instance = null;

    private final Map<String, Color> stateColors = null;
    private final List<QName> visibleElements = null;
    private final Map<QName, ImageIcon> elementIcons = null;
    private final Map<String, ImageIcon> stateEmblems = null;

    private NullConfiguration() {
        /* do nothing */
    }

    /**
     * Get the null-configuration instance.
     * @return the instance.
     */
    public static Configuration getInstance() {
        if (instance == null)
            instance = new NullConfiguration();

        return instance;
    }

    /* (non-Javadoc)
     * @see com.beepell.ui.dom.Configuration#getStateColors()
     */
    public Map<String, Color> getStateColors() {
        return this.stateColors;
    }

    /* (non-Javadoc)
     * @see com.beepell.ui.dom.Configuration#getVisibleElements()
     */
    public List<QName> getVisibleElements() {
        return this.visibleElements;
    }

    /* (non-Javadoc)
     * @see com.beepell.ui.dom.Configuration#getElementIcons()
     */
    public Map<QName, ImageIcon> getElementIcons() {
        return this.elementIcons;
    }

    /* (non-Javadoc)
     * @see com.beepell.ui.dom.Configuration#getStateEmblems()
     */
    public Map<String, ImageIcon> getStateEmblems() {
        return this.stateEmblems;
    }

    /* (non-Javadoc)
     * @see com.beepell.ui.dom.graph.Configuration#getBorderElements()
     */
    @Override
    public List<QName> getBorderElements() {
        return null;
    }

}
