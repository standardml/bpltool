package dk.itu.bpl.dix.frontend;

import dk.itu.bpl.dix.reactivexml.*;
import java.util.*;
import org.planx.xmlstore.*;

public class ReactionNode {
    private int index;
    private CompleteWideMatch m;
    private String name;
    
    public ReactionNode(String name, int index, CompleteWideMatch m){
        this.index = index;
        this.m = m;
        this.name = name;
    }
    
    
    /**
     * @return Returns the index.
     */
    public int getIndex() {
        return index;
    }
    /**
     * @return Returns the name.
     */
    public String getName() {
        return name;
    }
    public String toString(){
        int no = index +1;
        StringBuffer buf = new StringBuffer();
        buf.append("Reaction " + no);
        return buf.toString();
    }
}
