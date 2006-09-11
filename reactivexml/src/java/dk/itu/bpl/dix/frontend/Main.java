package dk.itu.bpl.dix.frontend;

import org.planx.xmlstore.*;
import org.planx.xmlstore.stores.*;
import org.planx.xmlstore.references.*;
import dk.itu.bpl.dix.store.*;
import java.io.*;
import java.net.*;
import javax.swing.*;
import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.PropertyConfigurator;

public class Main {

  private static DistributedReactiveStore setUpStore(Configuration conf) {
    String xsn = conf.getXMLStoreName();
    int udp = conf.getUDPPort();
    int tcp = conf.getTCPPort();
    InetSocketAddress bootstrap = null;
    if (conf.getBootstrapHost() != null) {
      System.out.println("BOOTSTRAP: " + conf.getBootstrapHost());
      bootstrap = new InetSocketAddress(conf.getBootstrapHost(), conf
          .getBootstrapPort());
    }
    try {
      XMLStore<ValueReference> local = new TranslatorXMLStore(
          new LocalXMLStore(xsn));
      XMLStore<ValueReference> xs = new DistributedXMLStore(local, udp, tcp,
          bootstrap);
      DistributedReactiveStore drs = new DistributedReactiveStore<ValueReference>(
          xs, xs.getNameServer());

      String n = conf.getXMLStoreName();
      String[] toCleanUp = new String[] { n + ".data", n + ".free",
          n + ".localns.map", n + ".trans.map", n + ".map", n + ".id" };
      Runtime.getRuntime().addShutdownHook(new CleanUp(xs, conf, toCleanUp));

      return drs;
    } catch (IOException e) {
      e.printStackTrace();
      System.out.println("Could not setup XML Store - exiting");
      System.exit(1);
      return null;
    }
  }

  public static void main(String args[]) {
    Configuration conf = new Configuration(args);
    
    System.out.println("Logger file: " + conf.getLoggerConfFile());
    
    //Configuration of Loggers
    PropertyConfigurator.configure(conf.getLoggerConfFile());
    
    //Get sax driver for OS X
    if ("Mac OS X".equals(System.getProperty("os.name"))) {
        System.setProperty("org.xml.sax.driver",
        "org.apache.crimson.parser.XMLReaderImpl");

    }

    
    DistributedReactiveStore drs = setUpStore(conf);

    if (conf.getGUI()) {
      try {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      } catch (Exception e) {
      }
      try {
        new DistReactGui(conf, drs);
      } catch (Exception e) {
        e.printStackTrace();
      }
    } else {
      try {
        CLIF clif = new CLIF(conf, drs);
        clif.MenuLoop();
        System.exit(0);
      } catch (IOException e) {
        System.out.println("Couldn't set up interactive loop - exiting");
      }
    }

  }
}
