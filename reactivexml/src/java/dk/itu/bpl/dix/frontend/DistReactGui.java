package dk.itu.bpl.dix.frontend;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import org.apache.log4j.Logger;
import org.planx.xmlstore.*;
import org.planx.xmlstore.input.SAXBuilder;
import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;

import dk.itu.bpl.dix.reactivexml.MatchFunctions;
import dk.itu.bpl.dix.reactivexml.RewriteRuleSet;
import dk.itu.bpl.dix.store.*;

public class DistReactGui extends JFrame implements ActionListener {
  
  private static Logger logger = Logger.getLogger(DistReactGui.class);

  private ReactiveStore store;
  private RewriteRuleSet rws = null;
  private Node rewriteRules = null;
  private Configuration conf;
  private String lastVisitedDir = "~/";

  //Center
  private ProcessTree ptree;
  private JPanel ptreepanel;
  //private XMLText ptree;
  private ReactionTree reactions;
  private JSplitPane center;

  //Buttons
  /*private JPanel buttons = new JPanel();*/
  private JButton fr = mkBut("Find24", "Find reactions", "Find");
  private JButton open = mkBut("Open24", "Load process tree", "Open");
  private JButton update = mkBut("Refresh24", "Refresh process tree", "Refresh");
  private JButton react = mkBut("History24", "Perform reaction", "Perform");
  private JButton loadRwr = mkBut("Import24", "Load rewrite rule set",
      "Load (RR)");

  //Menu
  private JMenuBar menuBar = new JMenuBar();
  private JMenu storeMenu = new JMenu("File");
  private JMenuItem loadPT = new JMenuItem("Load (PT)");
  private JMenuItem loadRR = new JMenuItem("Load (RR)");
  private JRadioButtonMenuItem clean = new JRadioButtonMenuItem(
      "Remove store on exit");
  
  private JMenuItem quit = new JMenuItem("Quit");

  // Toolbar
  private JToolBar toolBar = new JToolBar();

  private boolean removeStore = false;
  private String[] filesToRemove;

  public DistReactGui(Configuration conf, ReactiveStore store) {
    super("Distributed Reactive Store");
    this.store = store;
    this.conf = conf;

    String process = conf.getProcessTree();
    String rewrites = conf.getRewriteRules();
    if (process != null) {
      try {
        store.init(process);
      } catch (Exception e) {
        JOptionPane.showMessageDialog(this, "Could not load process tree "
            + process);
      }
    }
    if (rewrites != null) {
      try {
        rewriteRules = SAXBuilder.build(rewrites);
        rws = new RewriteRuleSet(rewriteRules, store);
      } catch (Exception e) {
        JOptionPane.showMessageDialog(this, "Could not load rewrite rules "
            + rewrites);
      }
    }

    setDefaultCloseOperation(EXIT_ON_CLOSE);

    Container pane = this.getContentPane();
    pane.setLayout(new BorderLayout());
    pane.add(toolBar, BorderLayout.NORTH);

    //Buttons
    /*
     GridLayout gl = new GridLayout(2,2);
     gl.setHgap(20);
     gl.setVgap(5);
     buttons.setLayout(gl);
     */
    fr.addActionListener(this);
    open.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        loadProcessTree();
      }
    });
    update.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        updateProcessTree();
      }
    });
    react.addActionListener(this);
    loadRwr.addActionListener(this);
    /*
     buttons.add(loadRwr);
     buttons.add(fr);
     buttons.add(update);
     buttons.add(react);
     */

    //Center
    center = new JSplitPane();
    try {
      ptree = new ProcessTree(store.getProcessTree());
    } catch (StoreNotInitializedException e) {
      ptree = new ProcessTree();
    } catch (IOException ioe) {
      JOptionPane.showMessageDialog(this, "IOException - Couldn't load process tree");
      ioe.printStackTrace();
    }
    ptree.setPreferredSize(new Dimension(300, 363));
    ptreepanel = new JPanel(new BorderLayout());
    ptreepanel.add(new JLabel("Process tree"), BorderLayout.NORTH);
    ptreepanel.add(ptree);
    center.setLeftComponent(ptreepanel);

    reactions = new ReactionTree();
    reactions.setPreferredSize(new Dimension(300, 363));
    JPanel reactionpanel = new JPanel(new BorderLayout());
    reactionpanel.add(new JLabel("Reaction rules"), BorderLayout.NORTH);
    reactionpanel.add(reactions);
    center.setRightComponent(reactionpanel);
    pane.add(center, BorderLayout.CENTER);
   

    //MenuBar
    menuBar.add(storeMenu);
    storeMenu.add(loadPT);
    storeMenu.add(loadRR);
    storeMenu.addSeparator();
    storeMenu.add(clean);
    storeMenu.addSeparator();
    storeMenu.add(quit);
    loadPT.addActionListener(this);
    loadRR.addActionListener(this);
    quit.addActionListener(this);
    clean.addActionListener(this);
    this.setJMenuBar(menuBar);

    //ToolBar
    toolBar.add(open);
    toolBar.add(update);
    toolBar.addSeparator();
    toolBar.add(loadRwr);
    toolBar.add(fr);
    toolBar.add(react);

    //this.pack();
    this.setSize(512, 600);
    this.setVisible(true);
    center.setDividerLocation(0.5);

    //Remove store
    clean.setSelected(true);
    conf.setDeleteStore();
  }

  private JButton mkBut(String img, String tip, String alt) {
    String imgloc = "images/" + img + ".gif";
    JButton button = new JButton();
    button.setToolTipText(tip);
    ImageIcon imico = new ImageIcon(imgloc, alt);
    if (imico.getImageObserver() != null) {
      button.setIcon(imico);
    } else {
      button.setText(alt);
      System.err.println("Resource not found: " + imgloc);
    }

    return button;
  }

  private void loadProcessTree() {
    JFileChooser ichooser = new JFileChooser(lastVisitedDir);
    int result = ichooser.showOpenDialog(this);
    if (result == JFileChooser.APPROVE_OPTION) {
      try {
        File f = ichooser.getSelectedFile();
        String file = f.getCanonicalPath();
        lastVisitedDir = file;
        store.init(file);
      } catch (Exception ex) {
        JOptionPane.showMessageDialog(this, "Couldn't load process tree");
        ex.printStackTrace();
      }
    }
    updateProcessTree();
  }

  private void updateProcessTree() {
    try {
      ptree.updateTree(store.getProcessTree());
    } catch (Exception ex) {
      JOptionPane.showMessageDialog(this, "No process tree loaded!");
      ex.printStackTrace();
    }
  }

  public void actionPerformed(ActionEvent e) {
    if (e.getSource() == update) {
      updateProcessTree();
    } else if (e.getSource() == fr) {
      if (rewriteRules == null) {
        JOptionPane.showMessageDialog(this, "No rewriterules loaded!!");
      } else {
        try {
          reactions.updateReactions(rws.findAllReactions());
          //reactions.repaint();//this.getContentPane().repaint();
        } catch (Exception ex) {
          logger.debug("Got exception:", ex);
          //ex.printStackTrace();
          JOptionPane.showMessageDialog(this, "Got exception: " + ex.getMessage());
        }
      }
    } else if (e.getSource() == react) {
      DefaultMutableTreeNode node = reactions.getSelected();
      Object info = node.getUserObject();
      if (info == null) {
        JOptionPane.showMessageDialog(this, "You must select a reaction");
      }
      try {
        ReactionNode rn = (ReactionNode) info;
        rws.performReaction(rn.getName(), rn.getIndex());
        reactions.cleanTree();
        updateProcessTree();
      } catch (ConflictException ce) {
        JOptionPane
            .showMessageDialog(this,
                "Reaction not performed!\nThere is a Conflict with another performed reaction");
      } catch (ClassCastException cex) {
        JOptionPane.showMessageDialog(this, "You must select a reaction");
      } catch (Exception ex) {
        ex.printStackTrace();
        JOptionPane.showMessageDialog(this, ex.toString() + ex.getMessage());
      }
    } else if ((e.getSource() == loadRwr) || (e.getSource() == loadRR)) {
      JFileChooser chooser = new JFileChooser(lastVisitedDir);
      int res = chooser.showOpenDialog(this);
      if (res == JFileChooser.APPROVE_OPTION) {
        try {
          String file = chooser.getSelectedFile().getCanonicalPath();
          lastVisitedDir = file;
          rewriteRules = SAXBuilder.build(file);
          rws = new RewriteRuleSet(rewriteRules, store);
        } catch (Exception ex) {
          JOptionPane.showMessageDialog(this, ex.getMessage());
        }
      }
    } else if (e.getSource() == clean) {
      if (clean.isSelected()) {
        clean.setSelected(true);
        conf.setDeleteStore();
      } else {
        clean.setSelected(false);
        conf.clearDeleteStore();
      }
    } else if (e.getSource() == quit) {
      System.exit(0);
    }

  }

}
