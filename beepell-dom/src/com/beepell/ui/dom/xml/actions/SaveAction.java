package com.beepell.ui.dom.xml.actions;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileWriter;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import com.beepell.ui.dom.xml.DocumentTextArea;
import com.beepell.ui.icon.IconRepository;

/**
 * Action to save (write out / serialize) to file the content of a
 * DocumentTextArea. Note: this re-serialize the complete document, independent
 * of the serialization used to display the xml in the text area.
 * 
 * @author Tim Hallwyl
 * 
 */
public class SaveAction extends AbstractAction {

    private static final long serialVersionUID = 1L;

    private static final FileFilter xml = new FileFilter() {

        @Override
        public boolean accept(File f) {
            return f.getName().toLowerCase().endsWith(".xml");
        }

        @Override
        public String getDescription() {
            return "XML"; // - Bitmap";
        }
    };

    private final JFileChooser fileChooser;

    private final DocumentTextArea documentTextArea;

    /**
     * Create a "save xml in document text area to file" action.
     * 
     * @param documentTextArea document text are to be saved.
     */
    public SaveAction(DocumentTextArea documentTextArea) {
        ImageIcon large = IconRepository.getIcon(IconRepository.ACTIONS, "filesave", IconRepository.MEDIUM);
        ImageIcon small = IconRepository.getIcon(IconRepository.ACTIONS, "filesave", IconRepository.TINY);

        this.putValue(LARGE_ICON_KEY, large);
        this.putValue(SMALL_ICON, small);
        this.putValue(NAME, "Save");
        this.putValue(SHORT_DESCRIPTION, "Save the XML document to file");
        this.putValue(LONG_DESCRIPTION, "Save the XML document to file");

        this.setEnabled(true);

        File home = new File(System.getProperty("user.home"));
        this.fileChooser = new JFileChooser(home);
        this.fileChooser.setAcceptAllFileFilterUsed(true);
        this.fileChooser.addChoosableFileFilter(xml);
        this.documentTextArea = documentTextArea;

    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        Icon critical = IconRepository.getIcon(IconRepository.ACTIONS, "messagebox_critical", IconRepository.MEDIUM);
        try {

            int status = this.fileChooser.showSaveDialog(null);

            if (status == JFileChooser.APPROVE_OPTION) {
                File selectedFile = this.fileChooser.getSelectedFile();
                String format = this.fileChooser.getFileFilter().getDescription();

                if (selectedFile.getName().indexOf('.') == -1)
                    selectedFile = new File(selectedFile.getCanonicalFile() + "." + format.toLowerCase());

                FileWriter fileWriter = new FileWriter(selectedFile);
                Transformer serializer;
                serializer = TransformerFactory.newInstance().newTransformer();
                serializer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
                serializer.setOutputProperty(OutputKeys.INDENT, "yes");
                serializer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
                serializer.setOutputProperty("{http://xml.apache.org/xalan}indent-amount", "4");
                serializer.setOutputProperty(OutputKeys.METHOD, "xml");
                serializer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
                serializer.transform(new DOMSource(this.documentTextArea.getNode().getOwnerDocument()), new StreamResult(fileWriter));

                fileWriter.close();
            }

        } catch (Exception exception) {

            JOptionPane.showMessageDialog(this.documentTextArea, exception.getLocalizedMessage(), "Cannot save to file", JOptionPane.ERROR_MESSAGE, critical);

        }

    }

}
