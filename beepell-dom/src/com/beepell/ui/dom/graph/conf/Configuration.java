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
public interface Configuration {

    /**
     * Different states may be assigned different colors. This maps from
     * bpl:state to a color.
     * 
     * @return the stateColors
     */
    public abstract Map<String, Color> getStateColors();

    /**
     * Not all elements should be shown. For example "to" and "from" child
     * elements of the WS-BPEL assign element is not shown, only the assign
     * element itself. Only elements in this list are shown.
     * 
     * @return the visibleElements
     */
    public abstract List<QName> getVisibleElements();

    /**
     * Element may be assigned an image icon using this map. It may return null
     * if no element icons are used.
     * 
     * @return the elementIcons
     */
    public abstract Map<QName, ImageIcon> getElementIcons();

    /**
     * States may be assigned emblems (small icons on top of the element icons)
     * to indicate the state value. It may return null if no state emblems are
     * used.
     * 
     * @return the stateEmblems
     */
    public abstract Map<String, ImageIcon> getStateEmblems();

    /**
     * Some elements are encapsulation other elements in a way that may need to
     * be visualized, for example the Scope activity in WS-BPEL. This method
     * returns a list of element qnames on the elements that needs a sub-tree
     * border marking.
     * 
     * @return a List of QNames of Elements that have a border enclosing their sub-trees.
     */
    public abstract List<QName> getBorderElements();

}