/*
 * CLIF.java
 *
 * Created on January 28, 2005, 1:36 PM
 */

package dk.itu.bpl.dix.frontend;

import org.planx.xmlstore.*;
import org.planx.xmlstore.input.*;
import dk.itu.bpl.dix.store.*;
import dk.itu.bpl.dix.reactivexml.*;
import dk.itu.bpl.dix.util.*;
import java.io.*;
import java.util.*;

/**
 * @author mol
 */
public class CLIF {

	private DistributedReactiveStore drs;

	private String processTree;

	private RewriteRuleSet rws;

	private String rewriteRules;

	/** Creates a new instance of CLIF */
	public CLIF(Configuration conf, DistributedReactiveStore drs)
			throws IOException {
		this.rws = null;
		this.drs = drs;

		setProcessTree(conf.getProcessTree());
		setRewriteRules(conf.getRewriteRules());
	}

	private void setProcessTree(String filename) throws IOException {
		if (filename == null)
			return;
		processTree = filename;
		try {
			drs.init(filename);
		} catch (NameServerException na) {
			System.out.println("A process tree already exsists!");
		}
	}

	private void setRewriteRules(String filename) {
		if (filename == null)
			return;
		rewriteRules = filename;
		try {
			rws = new RewriteRuleSet(SAXBuilder.build(filename), drs);
		} catch (IOException e) {
			System.out.println("Rewrite rules could not be loaded - not set");
			rws = null;
		} catch (XMLException e) {
			System.out.println("Parse error reading rewrite rules - not set");
			rws = null;
		} catch (RewriteException e) {
		  System.out.println("Error while loading rewrite rules - not set");
      rws=null;
    } catch (RuleException e) {
      System.out.println("Error while loading rewrite rules - not set\n Error:" + e.getMessage());
      e.printStackTrace();
    }
	}

	public void MenuLoop() throws IOException {
		final String options = "Options: \n"
				+ " p         - print current processtree \n"
				+ " f         - find reactions \n"
				+ " l [file]? - load rewrite ruleset \n"
				+ " i [file]? - initialize store \n" + " q         - quit \n"
				+ "Make your choice!";
		final BufferedReader in = new BufferedReader(new InputStreamReader(
				System.in));
		String input;

		do {
			System.out.println("Settings: tree = " + processTree + "; rules = "
					+ rewriteRules);
			System.out.println(options);
			input = in.readLine().trim();
			if (input == null || input.length() == 0) {
				System.out.println("Please enter SOMETHING!");
			} else if ("p".equals(input)) {
				try {
					Node n = drs.getProcessTree();
					System.out.println(NodeUtil.printNode(n, ""));
				} catch (StoreNotInitializedException e) {
					System.out.println("No process tree in store");
				}
			} else if ("f".equals(input)) {
				if (rws == null) {
					System.out.println("No rewrite rules loaded...");
				} else {
					Hashtable<String, List<CompleteWideMatch>> reactions;
					try {
						reactions = rws.findAllReactions();
					} catch (RewriteException e) {
						System.out.println("No reactions found ...");
						continue;
					}
					Iterator<String> keys = reactions.keySet().iterator();
					Hashtable<Integer, String> rwr = new Hashtable<Integer, String>();
					int counter = 1;
					while (keys.hasNext()) {
						rwr.put(new Integer(counter++), keys.next());
					}
					Enumeration<Integer> no = rwr.keys();
					do {
						System.out.println("Rewrite rules:");
						while (no.hasMoreElements()) {
							Integer nu = no.nextElement();
							System.out.println(nu.intValue() + " - "
									+ rwr.get(nu) + " ("
									+ reactions.get(rwr.get(nu)).size()
									+ " active reactions)");
						}
						Integer sel = null;
						System.out
								.println("Please select a rewriterule to see active reactions - or m to return to main menu");
						input = in.readLine().trim();
						try {
							sel = new Integer(Integer.parseInt(input));
						} catch (NumberFormatException nfe) {
						}
						if (sel != null && !"m".equals(input)
								&& rwr.containsKey(sel)) {
							List<CompleteWideMatch> m = reactions.get(rwr.get(sel));
							if (m.size() > 0) {
								for (int i = 0; i < m.size(); i++) {
									System.out
											.println("----------------------------------------");
									System.out.println("Reaction no: "
											+ (i + 1) + "\n");
									System.out.println(m.get(i));
									System.out
											.println("----------------------------------------\n");
								}
								int s = Integer.MIN_VALUE;
								System.out
										.println("To perform an action, please enter its number. Enter m to return to main menu");
								do {
									if (s != Integer.MIN_VALUE) {
										System.out
												.println("Not a valid number - please try again");
									}
									input = in.readLine().trim();
									try {
										s = Integer.parseInt(input);
									} catch (NumberFormatException nfe) {
									}
								} while (!"m".equals(input)
										&& (s < 1 || s > m.size()));
								if (s != Integer.MIN_VALUE) {
									try {
										rws
												.performReaction(rwr.get(sel),
														s - 1);
									} catch (Exception e) {
										System.out
												.println("Error performing reaction - "
														+ e);
										e.printStackTrace();
									}
								}
								input = "m";
							} else {
								System.out
										.println("Rewriterule contains no active reactions");
							}
						}

					} while (!"m".equals(input));
				}
			} else if (input.startsWith("l")) {
				String name = input.substring(1).trim();
				if (name != null && name.length() != 0) {
					File f = new File(name);
					if (f != null && !f.exists()) {
						System.out
								.println("File dosen't exist - please enter another path!");
						continue;
					}
				} else {
					System.out
							.println("Enter path to rewrite rule set - or m to return to main menu");
					File f = null;
					do {
						if (f != null && !f.exists()) {
							System.out
									.println("File dosen't exist - please enter another path or m to return to main menu");
						}
						input = in.readLine().trim();
						f = new File(input);
					} while ((f == null || !f.exists()) && !"m".equals(input));
					name = input;
				}
				if (!"m".equals(name)) {
					setRewriteRules(name);
				}
			} else if (input.startsWith("i")) {
				String name = input.substring(1).trim();
				if (name != null && name.length() != 0) {
					File f = new File(name);
					if (f != null && !f.exists()) {
						System.out
								.println("File dosen't exist - please enter another path!");
						continue;
					}
				} else {
					System.out
							.println("Please enter path to XML document containing processtree - or m to return to main menu");
					File f = null;
					do {
						if (f != null && !f.exists()) {
							System.out
									.println("File dosen't exist - please enter another path or m to return to main menu");
						}
						input = in.readLine().trim();
						f = new File(input);
					} while ((f == null || !f.exists()) && !"m".equals(input));
					name = input;
				}
				if (!"m".equals(name)) {
					setProcessTree(name);
				}
			}
		} while (!"q".equals(input));
	}
}
