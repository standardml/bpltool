package dk.itu.bpl.dix.frontend;
import javax.swing.*;
import javax.swing.tree.*;
import java.util.*;

import org.planx.xmlstore.*;

import dk.itu.bpl.dix.reactivexml.*;

public class ReactionTree extends JScrollPane  {
    JTree tree = null;
    public ReactionTree() {
        tree = new JTree(new DefaultMutableTreeNode("Reactions"));
        setViewportView(tree);
    }
    
    public ReactionTree(Hashtable<String,List<CompleteWideMatch>> matches) {
        tree = createTree(matches);
        setViewportView(tree);
    }
    
    public void updateReactions(Hashtable<String,List<CompleteWideMatch>> rules) {
        tree = createTree(rules);
        setViewportView(tree);
    }
    
    public void cleanTree(){
      tree = new JTree(new DefaultMutableTreeNode("Reactions"));
      setViewportView(tree);
    }
    
    public DefaultMutableTreeNode getSelected() {
        return (DefaultMutableTreeNode) tree.getLastSelectedPathComponent();
    }
    
    private JTree createTree(Hashtable<String,List<CompleteWideMatch>> rules) {
        DefaultMutableTreeNode root = new DefaultMutableTreeNode("Reactions");
        DefaultMutableTreeNode rule = null;
        Enumeration<String> en = rules.keys();
        while(en.hasMoreElements()) {
            String name = en.nextElement();
            List<CompleteWideMatch> matches = rules.get(name);
            rule = new DefaultMutableTreeNode(name + " - " + matches.size() + " reaction(s)");
            root.add(rule);
            DefaultMutableTreeNode reaction = null;
            Iterator it = matches.iterator();
            for(int i =0; i < matches.size(); i++ ) {
                CompleteWideMatch m = matches.get(i);
                reaction = new DefaultMutableTreeNode(new ReactionNode(name, i, m));
                rule.add(reaction);
                DefaultMutableTreeNode variables = new DefaultMutableTreeNode("Variables");
                reaction.add(variables);
                Set<String> vars = m.getMatchStore().getVariables();
                for(String s : vars) {
                    variables.add(new DefaultMutableTreeNode(s + " = " + m.getMatchStore().getVariable(s)));
                }
                DefaultMutableTreeNode holes = new DefaultMutableTreeNode("Holes");
                reaction.add(holes);
                Enumeration<String> hn = m.getHoleMap().getMap().keys();
                while(hn.hasMoreElements()) {
                    String holeName = hn.nextElement();
                    DefaultMutableTreeNode hole = new DefaultMutableTreeNode(holeName);
                    holes.add(hole);
                    Iterator<Node> nodes = m.getHoleMap().getNodesMappedBy(holeName).iterator();
                    while(nodes.hasNext()) {
                        hole.add(new DefaultMutableTreeNode(nodeInOneLine(nodes.next())));
                    }
                }
                List<PrimeMatch> pmatches = m.getPrimeMatches();
                for(PrimeMatch pm : pmatches) {
                  Node loc = pm.getLocation();
                  StringBuffer buf = new StringBuffer();
                  buf.append("WIDE ");
                  //buf.append(k +1);
                  buf.append(": ");
                  
                  buf.append(loc.getNodeValue());
                  List<Attribute> attr = loc.getAttributes();
                  for(Attribute a : attr) {
                      buf.append(" " + a.getName() + "=" + a.getValue());
                  }
                  DefaultMutableTreeNode l = new DefaultMutableTreeNode(buf.toString());
                  reaction.add(l);
                }
                  
            }
        }
        JTree tree = new JTree(root);
        tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        return tree;
    }
    
    private String nodeInOneLine(Node n) {
        StringBuffer buf = new StringBuffer();
        buf.append("<" + n.getNodeValue());
        List<Attribute> attributes = n.getAttributes();
        for(int i = 0; i < attributes.size();i++) {
            buf.append(" " + attributes.get(i).getName() + "=" + "\"" + attributes.get(i).getValue() + "\"");
        }
        List<? extends Node> childs = n.getChildren();
        if(childs.size() == 0) {
            buf.append("/>");
        } else {
            buf.append(">");
            for(int i=0; i < childs.size(); i++) {
                buf.append(nodeInOneLine(childs.get(i)));
            }
            buf.append("<" + n.getNodeValue() + "/>");
        }
        return buf.toString();
    }
}
