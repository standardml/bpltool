package com.beepell.tools.explorer;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.UIManager;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.w3c.dom.Document;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import com.beepell.tools.explorer.document.DocumentPane;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;

/**
 * @author Tim Hallwyl
 */
public class XPathExplorer extends JFrame {

    private static final long serialVersionUID = 1L;

    private final JTabbedPane tabbedPane;

    private static final JTextArea logTextArea = new JTextArea();;

    private static final JScrollPane logScrollPane = new JScrollPane(logTextArea);

    /**
     * 
     */
    @SuppressWarnings("serial")
    public XPathExplorer() {

        Action open = new AbstractAction("Open...") {

            public void actionPerformed(ActionEvent e) {
                final JFileChooser fc = new JFileChooser();
                int returnVal = fc.showOpenDialog(XPathExplorer.this);
                if (returnVal == JFileChooser.APPROVE_OPTION)
                    open(fc.getSelectedFile());
            }

        };

        Action save = new AbstractAction("Save") {

            public void actionPerformed(ActionEvent e) {
                save();
            }

        };

        Action saveAs = new AbstractAction("Save As...") {

            public void actionPerformed(ActionEvent e) {
                saveAs();
            }

        };

        Action close = new AbstractAction("Close") {

            public void actionPerformed(ActionEvent e) {
                if (tabbedPane.getSelectedComponent() != null)
                    tabbedPane.remove(tabbedPane.getSelectedComponent());
            }
        };

        Action applyStyle = new AbstractAction("Apply style sheet...") {

            public void actionPerformed(ActionEvent e) {
                final JFileChooser fc = new JFileChooser(new File("/home/hallwyl/.workspace/beepell/schemas"));
                int returnVal = fc.showOpenDialog(XPathExplorer.this);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    // open(fc.getSelectedFile());
                    try {
                        if (returnVal == JFileChooser.APPROVE_OPTION) {

                            if (tabbedPane.getSelectedComponent() != null) {

                                Document selected = ((DocumentPane) tabbedPane.getSelectedComponent()).getDocument();
                                Transformer transformer = TransformerFactory.newInstance().newTransformer(new StreamSource(fc.getSelectedFile()));

                                DOMResult result = new DOMResult();
                                transformer.transform(new DOMSource(selected), result);
                                Document transformed = (Document) result.getNode();
                                DocumentPane pane = new DocumentPane(transformed);
                                tabbedPane.add("Result", pane);
                                tabbedPane.setSelectedComponent(pane);

                            }
                        }
                    } catch (Exception exception) {
                        JOptionPane.showMessageDialog(tabbedPane, "Could not apply style sheet.");
                        log(exception.getLocalizedMessage());
                    }

                }
            }
        };

        
        Action validate = new AbstractAction("Apply schema...") {

            public void actionPerformed(ActionEvent e) {
                final JFileChooser fc = new JFileChooser(new File("/home/hallwyl/.workspace/beepell/schemas"));
                int returnVal = fc.showOpenDialog(XPathExplorer.this);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        if (returnVal == JFileChooser.APPROVE_OPTION) {

                            if (tabbedPane.getSelectedComponent() != null) {
                                Document selected = ((DocumentPane) tabbedPane.getSelectedComponent()).getDocument();
                                Schema schema = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI).newSchema(fc.getSelectedFile());
                                Validator validator = schema.newValidator();
                                validator.setErrorHandler(new ErrorHandler(){
                                    private void lg(Exception exception) {
                                        log(exception.getLocalizedMessage());
                                        if (exception.getCause() != null)
                                            log(exception.getCause().getLocalizedMessage());
                                        
                                        JOptionPane.showMessageDialog(tabbedPane, "The docuiment is not valid.");
                                    }
                                    
                                    public void error(SAXParseException exception) throws SAXException {
                                        lg(exception);
                                    }

                                    public void fatalError(SAXParseException exception) throws SAXException {
                                        lg(exception);
                                    }

                                    public void warning(SAXParseException exception) throws SAXException {
                                        lg(exception);
                                    }
                                    
                                });

                                DOMResult result = new DOMResult();
                                validator.validate(new DOMSource(selected), result);
                                

                            }
                        }
                    } catch (Exception exception) {
                        JOptionPane.showMessageDialog(tabbedPane, "Could not apply the schema.");
                        log(exception.getLocalizedMessage());
                    }

                }
            }
        };        
        
        
        setSize(new Dimension(800, 600));
        setLocationByPlatform(true);
        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("XPath Explorer");

        JMenuBar menu = new JMenuBar();
        JMenu fileMenu = new JMenu("File");
        fileMenu.add(open);
        fileMenu.addSeparator();
        fileMenu.add(close);
        fileMenu.addSeparator();
        fileMenu.add(save);
        fileMenu.add(saveAs);
        menu.add(fileMenu);

        JMenu transformMenu = new JMenu("Transform");
        transformMenu.add(applyStyle);
        menu.add(transformMenu);

        JMenu validateMenu = new JMenu("Validate");
        validateMenu.add(validate);
        menu.add(validateMenu);
        
        this.setJMenuBar(menu);

        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true);
        splitPane.setDividerLocation(400);

        this.tabbedPane = new JTabbedPane();
        splitPane.setTopComponent(tabbedPane);

        logTextArea.setEditable(false);
        splitPane.setBottomComponent(logScrollPane);

        XPathComboBox path = new XPathComboBox();
        tabbedPane.addChangeListener(path);
        this.add(BorderLayout.NORTH, path);

        this.add(BorderLayout.CENTER, splitPane);
        // this.pack();

        // open(new
        // File("/home/hallwyl/.workspace/beepell/test/com/beepell/deployment/sef-invoke-test-case-result.xml"));
        open(new File("/home/hallwyl/.workspace/beepell/schemas/bpel.xsd"));

    }

    @SuppressWarnings("unused")
    private void validate(Document document) {

        try {
            Schema schema = getSchema(document.getDocumentElement().getNamespaceURI());
            Validator validator = schema.newValidator();
            validator.validate(new DOMSource(document));

        } catch (Exception e) {
            log(e.getLocalizedMessage());
        }

    }

    private Schema getSchema(String namespaceURI) throws SAXException {
        SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        // schema = schemaFactory.newSchema(new
        // File("/home/hallwyl/.workspace/beepell/specifications/wsbpel.xsd"));
        return schemaFactory.newSchema();
    }

    private void open(File file) {
        try {

            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setValidating(false);
            factory.setCoalescing(false);
            factory.setIgnoringComments(true);
            factory.setIgnoringElementContentWhitespace(true);
            factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, false);
            factory.setNamespaceAware(true);

            DocumentBuilder builder = factory.newDocumentBuilder();
            final Document document = builder.parse(file);
            document.normalizeDocument();

            //validate(document);

            DocumentPane documentPane = new DocumentPane(document);
            documentPane.setFile(file);
            tabbedPane.add(file.getName(), documentPane);
            tabbedPane.setSelectedComponent(documentPane);

        } catch (Exception exception) {
            log("Cannot open file " + file.getName());
            log(exception.getLocalizedMessage());
            exception.printStackTrace();
        }
    }

    

    private void save() {
        try {
            if (tabbedPane.getSelectedComponent() != null) {

                DocumentPane pane = (DocumentPane) tabbedPane.getSelectedComponent();

                if (pane.getFile() != null) {
                    XMLSerializer serializer = new XMLSerializer();
                    serializer.setOutputCharStream(new java.io.FileWriter(pane.getFile()));
                    serializer.serialize(pane.getDocument());
                } else {
                    saveAs();
                }

            } else {
                log("Failed to save to file.");
            }
        } catch (Exception exception) {
            JOptionPane.showMessageDialog(tabbedPane, "Could not apply style sheet.");
            log(exception.getLocalizedMessage());
        }
    }

    private void saveAs() {
        try {
            if (tabbedPane.getSelectedComponent() != null) {

                DocumentPane pane = (DocumentPane) tabbedPane.getSelectedComponent();

                final JFileChooser fc = new JFileChooser(new File("/home/hallwyl/.workspace/beepell/appendix"));
                int returnVal = fc.showSaveDialog(XPathExplorer.this);
                if (returnVal != JFileChooser.APPROVE_OPTION)
                    return;

                XMLSerializer serializer = new XMLSerializer();
                serializer.setOutputCharStream(new java.io.FileWriter(fc.getSelectedFile()));
                serializer.serialize(pane.getDocument());

            } else {
                log("Failed to save to file.");
            }
        } catch (Exception exception) {
            JOptionPane.showMessageDialog(tabbedPane, "Could not apply style sheet.");
            log(exception.getLocalizedMessage());
        }
    }

    /**
     * @param args
     */
    public static void main(String[] args) {

        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());

            java.awt.EventQueue.invokeLater(new Runnable() {

                public void run() {
                    new XPathExplorer().setVisible(true);
                }
            });
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    /**
     * @param message
     */
    public static void log(String message) {
        if (message == null || message.isEmpty())
            message = "Log entry without a message.";

        logTextArea.setText(logTextArea.getText() + message + "\n");

    }

}
