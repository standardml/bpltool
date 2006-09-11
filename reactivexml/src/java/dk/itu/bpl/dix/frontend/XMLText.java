package dk.itu.bpl.dix.frontend;

import org.planx.xmlstore.*;
import java.util.*;

import javax.swing.*;
public class XMLText extends JScrollPane {
	private JTextArea text;
	
	public XMLText(){
		text = new JTextArea();
		text.setEditable(false);
		this.setViewportView(text);
	}
	
	public XMLText(Node n) {
		this();
		updateProcessTree(n);
	}

	public void updateProcessTree(Node n) {
		text.setText(nodeToString(n, ""));
		repaint();
	}
	
	private String nodeToString(Node n, String tab) {
		StringBuffer buf = new StringBuffer();
		List<? extends Node> childs = n.getChildren();
		buf.append("\n" + tab);
		if(childs.size() == 0) {
			buf.append(" - ");
		} else {
			buf.append(" +");
		}
		buf.append(n.getNodeValue());
                List<Attribute> attributes = n.getAttributes();
		for(int i = 0; i < attributes.size();i++) {
			buf.append(" " + attributes.get(i).getName() + "=" + "\"" + attributes.get(i).getValue() + "\"");
		}
			for(int i=0; i < childs.size(); i++) {
				buf.append(nodeToString(childs.get(i), tab + "    "));
		}
		return buf.toString();
	}
}
