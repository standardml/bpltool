package com.beepell.tools.tracker;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.LogRecord;

import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.TableModel;



/**
 * @author Tim Hallwyl
 *
 */
public class LogHandler extends Handler implements TableModel {

    private List<LogRecord> records = new ArrayList<LogRecord>();
    private List<TableModelListener> listeners = new ArrayList<TableModelListener>();
    
    private String[] columns = new String[] {"Thread", "Time", "Level", "Message"};
    
    DateFormat format = new SimpleDateFormat("H:m:s.S"); 
    
    /* (non-Javadoc)
     * @see java.util.logging.Handler#close()
     */
    @Override
    public void close() throws SecurityException {
    }

    /* (non-Javadoc)
     * @see java.util.logging.Handler#flush()
     */
    @Override
    public void flush() {
    }

    /* (non-Javadoc)
     * @see java.util.logging.Handler#publish(java.util.logging.LogRecord)
     */
    @Override
    public void publish(LogRecord record) {
        if (record == null)
            return;
    
        records.add(record);
        
        for (TableModelListener listener : listeners) {
            listener.tableChanged(new TableModelEvent(this, records.size(), records.size(), TableModelEvent.ALL_COLUMNS, TableModelEvent.INSERT));
        }
    
    }

    public void addTableModelListener(TableModelListener l) {
        listeners.add(l);        
    }

    public Class<?> getColumnClass(int columnIndex) {
        switch (columnIndex) {
        case 0:
            return Integer.class;
        case 1:
            return String.class;
        case 2:
            return String.class;
        case 3:
            return String.class;
        default:
            return null;
        }
    }

    public int getColumnCount() {
        return columns.length;
    }

    public String getColumnName(int columnIndex) {
        return columns[columnIndex];
    }

    public int getRowCount() {
        return records.size();
    }
   
    public Object getValueAt(int rowIndex, int columnIndex) {

        switch (columnIndex) {
        case 0:
            return records.get(rowIndex).getThreadID();
        case 1:
            return format.format(new Date(records.get(rowIndex).getMillis()));
        case 2:
            return records.get(rowIndex).getLevel();
        case 3:
            return records.get(rowIndex).getMessage();
        default:
            return null;
        }

    }

    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return false;
    }

    public void removeTableModelListener(TableModelListener l) {
        listeners.remove(l);     
    }

    public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
    }

}
