package com.beepell.ui.dom.graph.conf;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.xml.namespace.QName;

import com.beepell.ui.icon.IconRepository;

/**
 * WS-BPEL Configuration of the DocumentGraphPanel
 * @author Tim Hallwyl
 * 
 */
public class BPELConfiguration implements Configuration {

    private static final String BPEL = "http://docs.oasis-open.org/wsbpel/2.0/process/executable";
    private static Configuration instance = null;

    private final Map<String, Color> stateColors = new HashMap<String, Color>(11);
    private final List<QName> visibleElements = new ArrayList<QName>(23);
    private final List<QName> borderElements = new ArrayList<QName>(1);
    private final Map<QName, ImageIcon> elementIcons = new HashMap<QName, ImageIcon>();
    private final Map<String, ImageIcon> stateEmblems = new HashMap<String, ImageIcon>();

    private static final QName process = new QName(BPEL, "process");
    private static final QName assign = new QName(BPEL, "assign");
    private static final QName compensate = new QName(BPEL, "compensate");
    private static final QName compensateScope = new QName(BPEL, "compensateScope");
    private static final QName empty = new QName(BPEL, "empty");
    private static final QName exit = new QName(BPEL, "exit");
    private static final QName invoke = new QName(BPEL, "invoke");
    private static final QName receive = new QName(BPEL, "receive");
    private static final QName reply = new QName(BPEL, "reply");
    private static final QName rethrow = new QName(BPEL, "rethrow");
    private static final QName _throw = new QName(BPEL, "throw");
    private static final QName validate = new QName(BPEL, "validate");
    private static final QName wait = new QName(BPEL, "wait");
    private static final QName flow = new QName(BPEL, "flow");
    private static final QName forEach = new QName(BPEL, "forEach");
    private static final QName _if = new QName(BPEL, "if");
    private static final QName elseif = new QName(BPEL, "elseif");
    private static final QName pick = new QName(BPEL, "pick");
    private static final QName repeatUntil = new QName(BPEL, "repeatUntil");
    private static final QName scope = new QName(BPEL, "scope");
    private static final QName sequence = new QName(BPEL, "sequence");
    private static final QName _while = new QName(BPEL, "while");
    
    private BPELConfiguration() {
        // blue group
        this.stateColors.put("running", new Color(102, 153, 255));
        this.stateColors.put("waiting", new Color(204, 204, 255));
        this.stateColors.put("ready", new Color(0, 102, 255));
        this.stateColors.put("initializing", new Color(0, 0, 255));
        // gray
        this.stateColors.put("completed", new Color(204, 204, 204));
        this.stateColors.put("skipped", new Color(204, 204, 204));
        // red
        this.stateColors.put("terminating", new Color(255, 51, 51));
        this.stateColors.put("terminated", new Color(255, 153, 153));

        // yellow
        this.stateColors.put("failing", new Color(255, 255, 102));
        this.stateColors.put("failed", new Color(255, 255, 102));
        this.stateColors.put("compensating", new Color(255, 255, 51));
        this.stateColors.put("compensated", new Color(255, 255, 153));

        this.borderElements.add(scope);
        
        this.visibleElements.add(process);
        this.visibleElements.add(assign);
        this.visibleElements.add(compensate);
        this.visibleElements.add(compensateScope);
        this.visibleElements.add(empty);
        this.visibleElements.add(exit);
        this.visibleElements.add(invoke);
        this.visibleElements.add(receive);
        this.visibleElements.add(reply);
        this.visibleElements.add(rethrow);
        this.visibleElements.add(_throw);
        this.visibleElements.add(validate);
        this.visibleElements.add(wait);
        this.visibleElements.add(flow);
        this.visibleElements.add(forEach);
        this.visibleElements.add(_if);
        this.visibleElements.add(elseif);
        this.visibleElements.add(pick);
        this.visibleElements.add(repeatUntil);
        this.visibleElements.add(scope);
        this.visibleElements.add(sequence);
        this.visibleElements.add(_while);
        
        //ImageIcon icon = IconRepository.getIcon(IconRepository.APPS, "applications_systemg", IconRepository.LARGE);
        this.elementIcons.put(process, IconRepository.getIcon(IconRepository.APPS, "applications_systemg", IconRepository.LARGE));
        this.elementIcons.put(assign, IconRepository.getIcon(IconRepository.ACTIONS, "revert", IconRepository.LARGE));
        this.elementIcons.put(compensate, IconRepository.getIcon(IconRepository.ACTIONS, "undo", IconRepository.LARGE));
        this.elementIcons.put(compensateScope, IconRepository.getIcon(IconRepository.ACTIONS, "undo", IconRepository.LARGE));
        this.elementIcons.put(empty, IconRepository.getIcon(IconRepository.MIMETYPES, "empty", IconRepository.LARGE));
        this.elementIcons.put(exit, IconRepository.getIcon(IconRepository.ACTIONS, "application_exit", IconRepository.LARGE));
        this.elementIcons.put(invoke, IconRepository.getIcon(IconRepository.ACTIONS, "invoke", IconRepository.LARGE));
        this.elementIcons.put(receive, IconRepository.getIcon(IconRepository.ACTIONS, "receive", IconRepository.LARGE));
        this.elementIcons.put(reply, IconRepository.getIcon(IconRepository.ACTIONS, "reply", IconRepository.LARGE));
        this.elementIcons.put(rethrow, IconRepository.getIcon(IconRepository.ACTIONS, "messagebox_warning", IconRepository.LARGE));
        this.elementIcons.put(_throw, IconRepository.getIcon(IconRepository.ACTIONS, "messagebox_warning", IconRepository.LARGE));
        this.elementIcons.put(validate, IconRepository.getIcon(IconRepository.ACTIONS, "apply", IconRepository.LARGE));
        this.elementIcons.put(wait, IconRepository.getIcon(IconRepository.ACTIONS, "alarmclock", IconRepository.LARGE));
        this.elementIcons.put(flow, IconRepository.getIcon(IconRepository.FILESYSTEMS, "document_multiple", IconRepository.LARGE));
        this.elementIcons.put(forEach, IconRepository.getIcon(IconRepository.ACTIONS, "rotate", IconRepository.LARGE));
        this.elementIcons.put(_if, IconRepository.getIcon(IconRepository.MIMETYPES, "unknown", IconRepository.LARGE));
        this.elementIcons.put(elseif, IconRepository.getIcon(IconRepository.MIMETYPES, "unknown", IconRepository.LARGE));
        this.elementIcons.put(pick, IconRepository.getIcon(IconRepository.ACTIONS, "transform_move", IconRepository.LARGE));
        this.elementIcons.put(repeatUntil, IconRepository.getIcon(IconRepository.ACTIONS, "rotate", IconRepository.LARGE));
        this.elementIcons.put(scope, IconRepository.getIcon(IconRepository.FILESYSTEMS, "folder", IconRepository.LARGE));
        this.elementIcons.put(sequence, IconRepository.getIcon(IconRepository.MIMETYPES, "make", IconRepository.LARGE));
        this.elementIcons.put(_while, IconRepository.getIcon(IconRepository.ACTIONS, "rotate", IconRepository.LARGE));
        
        this.stateEmblems.put("running", IconRepository.getIcon(IconRepository.EMBLEMS, "running", IconRepository.TINY));
        this.stateEmblems.put("waiting", IconRepository.getIcon(IconRepository.EMBLEMS, "hourglass", IconRepository.TINY));
        this.stateEmblems.put("ready", IconRepository.getIcon(IconRepository.EMBLEMS, "star", IconRepository.TINY));
        this.stateEmblems.put("initializing", IconRepository.getIcon(IconRepository.EMBLEMS, "star_off", IconRepository.TINY));
        this.stateEmblems.put("completed", IconRepository.getIcon(IconRepository.EMBLEMS, "ok_green", IconRepository.TINY));
        this.stateEmblems.put("skipped", IconRepository.getIcon(IconRepository.EMBLEMS, "cancel", IconRepository.TINY));
        this.stateEmblems.put("terminating", IconRepository.getIcon(IconRepository.EMBLEMS, "close", IconRepository.TINY));
        this.stateEmblems.put("terminated", IconRepository.getIcon(IconRepository.EMBLEMS, "close_gray", IconRepository.TINY));
        this.stateEmblems.put("failing", IconRepository.getIcon(IconRepository.EMBLEMS, "fault", IconRepository.TINY));
        this.stateEmblems.put("failed", IconRepository.getIcon(IconRepository.EMBLEMS, "fault", IconRepository.TINY));
        this.stateEmblems.put("compensating", IconRepository.getIcon(IconRepository.EMBLEMS, "redo", IconRepository.TINY));
        this.stateEmblems.put("compensated", IconRepository.getIcon(IconRepository.EMBLEMS, "star_off", IconRepository.TINY));
        
    }


    /**
     * Get the BPEL configuration instance.
     * @return The BPEL configuration.
     */
    public static Configuration getInstance() {
        if (instance == null)
            instance = new BPELConfiguration();

        return instance;
    }

    /* (non-Javadoc)
     * @see com.beepell.ui.dom.Configuration#getStateColors()
     */
    public Map<String, Color> getStateColors() {
        return this.stateColors;
    }

    /* (non-Javadoc)
     * @see com.beepell.ui.dom.Configuration#getVisibleElements()
     */
    public List<QName> getVisibleElements() {
        return this.visibleElements;
    }

    /* (non-Javadoc)
     * @see com.beepell.ui.dom.Configuration#getElementIcons()
     */
    public Map<QName, ImageIcon> getElementIcons() {
        return this.elementIcons;
    }

    /* (non-Javadoc)
     * @see com.beepell.ui.dom.Configuration#getStateEmblems()
     */
    public Map<String, ImageIcon> getStateEmblems() {
        return this.stateEmblems;
    }

    /* (non-Javadoc)
     * @see com.beepell.ui.dom.graph.Configuration#getBorderElements()
     */
    @Override
    public List<QName> getBorderElements() {
        return this.borderElements;
    }

}
