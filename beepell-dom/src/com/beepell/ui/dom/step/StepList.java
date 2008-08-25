package com.beepell.ui.dom.step;

import javax.swing.JTable;
import javax.swing.event.TableModelListener;
import javax.swing.table.TableModel;

/**
 * Dummy Step List implementation.
 * 
 * @author Tim Hallwyl
 *
 */
public class StepList extends JTable {
    
    private static final long serialVersionUID = 1L;

    /**
     * Dummy implementation
     */
    public StepList() {
        
        super(new TableModel() {

            //ImageIcon icon = IconRepository.getIcon(IconRepository.ACTIONS, "footprint", IconRepository.LARGE);
            //JLabel label = new JLabel(this.icon);
            
            
            @Override
            public void addTableModelListener(TableModelListener l) {
                // TODO Auto-generated method stub
                
            }

            @Override
            public Class<?> getColumnClass(int columnIndex) {
                return String.class;
            }

            @Override
            public int getColumnCount() {
                return 1;
            }

            @Override
            public String getColumnName(int columnIndex) {
                return "Steps";
            }

            @Override
            public int getRowCount() {
                return 120;
            }

            @Override
            public Object getValueAt(int rowIndex, int columnIndex) {
                return "Step " + rowIndex;
            }

            @Override
            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return false;
            }

            @Override
            public void removeTableModelListener(TableModelListener l) {
                // TODO Auto-generated method stub
            }

            @Override
            public void setValueAt(Object value, int rowIndex, int columnIndex) {
                // TODO Auto-generated method stub
            }
            
        });

        this.setShowGrid(false);
        
        this.getColumnModel().getColumn(0).setCellRenderer(new StepCellRenderer());
        this.setRowHeight(36);
        
    }

}
