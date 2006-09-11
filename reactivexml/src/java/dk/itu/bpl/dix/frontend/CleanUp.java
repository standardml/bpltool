package dk.itu.bpl.dix.frontend;

import org.planx.xmlstore.*;
import org.planx.xmlstore.references.*;
import java.io.*;

public class CleanUp extends Thread {
    private String[] cleanup;
    private XMLStore<ValueReference> xs;
    private Configuration conf;

    public CleanUp(XMLStore<ValueReference> xs, Configuration conf,
		   String[] files) {
	this.cleanup = files;
	this.xs = xs;
	this.conf = conf;
    }

    public void run() {
	System.out.println("Cleaning up");
	try{
	    xs.close();
	} catch (IOException e) {
	    System.out.println("Could not close store!!");
	}
	boolean ok = false;
	final BufferedReader in = 
	    new BufferedReader(new InputStreamReader(System.in));

	String answ = null;
	if(!conf.getGUI()) {
	    while(!ok) {
		System.out.println("\nDelete local store? (enter yes/no)");
		try{
		    answ = in.readLine().trim();
		} catch (IOException e) {}
		if("yes".equals(answ)) {
		    ok = true;
		} else if("no".equals(answ)) {
		    ok = true;
		}
	    }
	} else if(conf.getDeleteStore()) {
	    answ = "yes";
	}
	if("yes".equals(answ)) {
	    for(int i = 0; i < cleanup.length; i++) {
		new File(cleanup[i]).delete();
	    }
	}
	try{
	    in.close();
	} catch (IOException e) {}                
	//System.exit(0);
    }
}

