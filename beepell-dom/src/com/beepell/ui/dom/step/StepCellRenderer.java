package com.beepell.ui.dom.step;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.UIManager;
import javax.swing.table.TableCellRenderer;

import com.beepell.ui.icon.IconRepository;

/**
 * TODO: Implement this class.
 * @author Tim Hallwyl
 * 
 */
public class StepCellRenderer extends JComponent implements TableCellRenderer {

    private static final long serialVersionUID = 1L;
    
    private static final ImageIcon icon = IconRepository.getIcon(IconRepository.ACTIONS, "footprint", IconRepository.MEDIUM);

    private static final Font regular = new Font("FreeSans", Font.PLAIN, 12);

    private static final Font bold = new Font("FreeSans", Font.BOLD, 12);
    
    private Color textColor;
    
    /**
     * Text line written in bold.
     */
    private String title;
    
    /**
     * Text line written with regular type.
     */
    private String description;
    
    /**
     * Get the step table cell renderer.
     * 
     * @param table
     * @param step
     * @param isSelected
     * @param hasFocus
     * @param row
     * @param column
     * @return a the step table cell renderer.
     */
    public Component getTableCellRendererComponent(JTable table, Object step, boolean isSelected, boolean hasFocus, int row, int column) {
        if (row % 2 == 0)
            setBackground(Color.LIGHT_GRAY);
        else
            setBackground(Color.WHITE);
        
        if (isSelected) {            
            setBorder(BorderFactory.createLineBorder(Color.BLUE, 1));
            setBackground(UIManager.getColor("Table.selectionBackground"));
            this.textColor = UIManager.getColor("Table.selectionForeground");
            
        } else {
            setBackground(UIManager.getColor("Table.background"));
            this.textColor = UIManager.getColor("Table.foreground");
            setBorder(null);
        }
        
        this.title = ("Step " + row);
        this.description = "Another step down the long path to salvation...";
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.swing.JComponent#paint(java.awt.Graphics)
     */
    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2d = (Graphics2D) g;
        
        // draw background
        Rectangle clip = g2d.getClipBounds();
        g2d.setColor(getBackground());
        g2d.fillRect(clip.x, clip.y, clip.width, clip.height);

        // Draw icon
        g2d.drawImage(icon.getImage(), 2, 2, null);
        
        // Draw text
        g2d.setColor(this.textColor);
        FontMetrics metrics = g2d.getFontMetrics(bold);
        g2d.setFont(bold);
        g2d.drawString(this.title, 38, metrics.getAscent() + 2);
        g2d.setFont(regular);
        g2d.drawString(this.description, 38, metrics.getHeight() + metrics.getAscent());
    }
    
}
