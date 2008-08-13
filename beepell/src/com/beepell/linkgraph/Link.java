package com.beepell.linkgraph;

import com.beepell.model.Activity;
import com.beepell.model.ScopeActivity;


/**
 * @author Tim Hallwyl
 *
 */
public class Link {

    /**
     * 
     */
    public final String name;
    
    /**
     * 
     */
    public Activity source;
    
    /**
     * 
     */
    public Activity target;
    
    /**
     * 
     */
    public ScopeActivity sourceScope;
    
    /**
     * 
     */
    public ScopeActivity targetScope;
    
    /**
     * 
     * @param name
     */
    public Link(String name) {
        this.name = name;
    }
}
