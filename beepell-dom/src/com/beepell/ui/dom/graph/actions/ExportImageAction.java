package com.beepell.ui.dom.graph.actions;

import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;

import javax.imageio.ImageIO;
import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.filechooser.FileFilter;

import net.sf.epsgraphics.ColorMode;
import net.sf.epsgraphics.EpsGraphics;

import com.beepell.ui.icon.IconRepository;

/**
 * 
 * @author Tim Hallwyl
 * 
 */
public class ExportImageAction extends AbstractAction {

    private static final long serialVersionUID = 1L;

    private static final FileFilter bmp = new FileFilter() {

        @Override
        public boolean accept(File f) {
            return f.getName().toLowerCase().endsWith(".bmp");
        }

        @Override
        public String getDescription() {
            return "BMP"; // - Bitmap";
        }
    };

    private static final FileFilter gif = new FileFilter() {

        @Override
        public boolean accept(File f) {
            return f.getName().toLowerCase().endsWith(".gif");
        }

        @Override
        public String getDescription() {
            return "GIF"; // - Graphics Interchange Format";
        }
    };

    private static final FileFilter jpeg = new FileFilter() {

        @Override
        public boolean accept(File f) {
            return f.getName().toLowerCase().endsWith(".jpeg") || f.getName().toLowerCase().endsWith(".jpg");
        }

        @Override
        public String getDescription() {
            return "JPEG"; // - Joint Photographic Experts Group";
        }
    };

    private static final FileFilter png = new FileFilter() {

        @Override
        public boolean accept(File f) {
            return f.getName().toLowerCase().endsWith(".png");
        }

        @Override
        public String getDescription() {
            return "PNG"; // - Portable Network Graphics";
        }
    };

    private static final FileFilter eps = new FileFilter() {

        @Override
        public boolean accept(File f) {
            return f.getName().toLowerCase().endsWith(".eps");
        }

        @Override
        public String getDescription() {
            return "EPS"; // - Encapsulated PostScript";
        }
    };

    private final JFileChooser fileChooser;

    private final JPanel panel;

    /**
     * Create a "save panel as graphic file" action.
     * @param panel the JPanel to save as file.
     */
    public ExportImageAction(JPanel panel) {
        ImageIcon large = IconRepository.getIcon(IconRepository.ACTIONS, "filesave", IconRepository.MEDIUM);
        ImageIcon small = IconRepository.getIcon(IconRepository.ACTIONS, "filesave", IconRepository.TINY);

        this.putValue(LARGE_ICON_KEY, large);
        this.putValue(SMALL_ICON, small);
        this.putValue(NAME, "Save Graph as Image");
        this.putValue(SHORT_DESCRIPTION, "Save the graph to image file");
        this.putValue(LONG_DESCRIPTION, "Save the graph tree as an image file, such as PNG, JPEG ect.");

        this.setEnabled(true);

        File home = new File(System.getProperty("user.home"));
        this.fileChooser = new JFileChooser(home);
        this.fileChooser.setAcceptAllFileFilterUsed(false);
        this.fileChooser.addChoosableFileFilter(bmp);
        this.fileChooser.addChoosableFileFilter(gif);
        this.fileChooser.addChoosableFileFilter(jpeg);
        this.fileChooser.addChoosableFileFilter(png);
        this.fileChooser.addChoosableFileFilter(eps);
        this.panel = panel;

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
                Dimension size = this.panel.getSize();

                if (selectedFile.getName().indexOf('.') == -1)
                    selectedFile = new File(selectedFile.getCanonicalFile() + "." + format.toLowerCase());
                
                if (format.equals("EPS")) {
                    PrintStream stream = new PrintStream(selectedFile);
                    EpsGraphics g2d = new EpsGraphics("Instance", stream, 0, 0, size.width + 60, size.height + 60, ColorMode.COLOR_CMYK);
                    this.panel.paint(g2d);
                    g2d.close();
                    stream.close();
                } else {
                    BufferedImage bi = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_RGB);
                    Graphics2D ig2 = bi.createGraphics();
                    this.panel.paint(ig2);
                    if (!ImageIO.write(bi, format, selectedFile)) {
                        JOptionPane.showMessageDialog(null, "Could not save the image as " + format + ".", "Error", JOptionPane.ERROR_MESSAGE, critical);
                    }
                }
            }

        } catch (IOException exception) {

            JOptionPane.showMessageDialog(this.panel, exception.getLocalizedMessage(), "Cannot save to file", JOptionPane.ERROR_MESSAGE, critical);

        }

    }

}
