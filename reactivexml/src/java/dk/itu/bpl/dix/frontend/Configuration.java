package dk.itu.bpl.dix.frontend;

import org.apache.commons.cli.*;

public class Configuration {
  private String name;
  private int tcp;
  private int udp;
  private String bootstrap_host;
  private int bootstrap_port;
  private String process_tree;
  private String rewrite_rules;
  private boolean gui;
  private boolean delete;
  private String loggerConf = "src/log4j.properties";

  public Configuration(String[] args) {
    // Apache Commons CLI command-line parsing
    Options opts = new Options();
    opts.addOption(OptionBuilder.withArgName("name").hasArg().withDescription(
        "local XMLStore name").withLongOpt("name").create("n"));
    opts.addOption(OptionBuilder.withArgName("port").hasArg().withDescription(
        "UDP port for routing protocol").withLongOpt("udp").create("u"));
    opts.addOption(OptionBuilder.withArgName("port").hasArg().withDescription(
        "TCP port for data protocol").withLongOpt("tcp").create("t"));
    opts.addOption(OptionBuilder.withArgName("host").hasArg().withDescription(
        "bootstrap host name").withLongOpt("bootstrap").create("b"));
    opts.addOption(OptionBuilder.withArgName("port").hasArg().withDescription(
        "bootstrap host port").withLongOpt("bootstrap-port").create("B"));
    opts.addOption(OptionBuilder.withArgName("file").hasArg().withDescription(
            "file containing process tree").withLongOpt("process-tree").create(
            "p"));
    opts.addOption(OptionBuilder.withArgName("file").hasArg().withDescription(
        "file containing rewrite rules").withLongOpt("rewrite-rules").create(
        "r"));
    opts.addOption(OptionBuilder.withDescription(
            "use GUI (default: use text interface)").withLongOpt("gui").create(
            "g"));
    opts.addOption(OptionBuilder.withArgName("logConf").hasArg().withDescription(
        "logger configuraion file").withLongOpt("logc").create("l"));


    CommandLineParser parser = new PosixParser();
    CommandLine cmdline = null;
    try {
      // parse the command line arguments
      cmdline = parser.parse(opts, args);

      if (!cmdline.hasOption("n") || !cmdline.hasOption("u")
          || !cmdline.hasOption("t")) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("reactive-xml", opts, true);
        System.exit(1);
      }
      if(cmdline.hasOption("l")){
        this.loggerConf = cmdline.getOptionValue("l");
      }
      this.udp = Integer.parseInt(cmdline.getOptionValue("u"));
      this.tcp = Integer.parseInt(cmdline.getOptionValue("t"));
      this.name = cmdline.getOptionValue("n");

      this.bootstrap_host = null;
      this.bootstrap_port = -1;
      if (cmdline.hasOption("b")) {
        if (cmdline.hasOption("B")) {
          this.bootstrap_host = cmdline.getOptionValue("b");
          this.bootstrap_port = Integer.parseInt(cmdline.getOptionValue("B"));
        } else {
          System.out
              .println("No bootstrap port specified - ignoring bootstrap host");
        }
      }

      this.process_tree = cmdline.getOptionValue("p");
      this.rewrite_rules = cmdline.getOptionValue("r");
      if (cmdline.hasOption("g"))
        this.gui = true;
      else
        this.gui = false;
    } catch (Exception exp) {
      System.out.println("Unexpected exception:" + exp.getMessage());
      HelpFormatter formatter = new HelpFormatter();
      formatter.printHelp("reactive-xml", opts);
      System.exit(1);
    }
  }

  public String getXMLStoreName() {
    return name;
  }

  public int getTCPPort() {
    return tcp;
  }

  public int getUDPPort() {
    return udp;
  }

  public String getBootstrapHost() {
    return bootstrap_host;
  }

  public int getBootstrapPort() {
    return bootstrap_port;
  }

  public String getProcessTree() {
    return process_tree;
  }

  public String getRewriteRules() {
    return rewrite_rules;
  }

  public boolean getGUI() {
    return gui;
  }

  public void setDeleteStore() {
    this.delete = true;
  }

  public void clearDeleteStore() {
    this.delete = false;
  }

  public boolean getDeleteStore() {
    return delete;
  }
  
  public String getLoggerConfFile(){
    return loggerConf;
  }
}
