package com.beepell.ui.icon;

import javax.swing.ImageIcon;

/**
 * A static utility class to load icon resources from repository.
 * 
 * @author Tim Hallwyl
 * 
 */
public class IconRepository {

    /**
     * 16 x 16 pixel (for menus and trees)
     */
    public final static String TINY = "16x16";

    /**
     * 22 x 22 pixel
     */
    public final static String SMALL = "22x22";

    /**
     * 32 x 32 pixel (for toolbars)
     */
    public final static String MEDIUM = "32x32";

    /**
     * 48 x 48 pixel
     */
    public final static String LARGE = "48x48";

    /**
     * 64 x 64 pixel
     */
    public final static String XLARGE = "64x64";

    /**
     * 128 x 128 pixel
     */
    public final static String HUGE = "128x128";

    /**
     * Actions (buttons)
     */
    public final static String ACTIONS = "actions";

    /**
     * Applications / Stock
     */
    public final static String APPS = "apps";

    /**
     * Devices
     */
    public final static String DEVICES = "devices";

    /**
     * Filesystems
     */
    public final static String FILESYSTEMS = "filesystems";

    /**
     * Mimitypes (file formats)
     */
    public final static String MIMETYPES = "mimetypes";

    /**
     * Emblems (small icons to place on top of other icons)
     */
    public final static String EMBLEMS = "emblems";

    /**
     * Get an icon from repository.
     * 
     * @param category the icon category, for example ACTIONS
     * @param name the icon name, for example "color_line"
     * @param size the icon size, for example LARGE
     * @return the requested icon or a default icon if the requested was not
     *         found.
     */
    public final static ImageIcon getIcon(String category, String name, String size) {
        try {
            String path = size + "/" + category + "/" + name + ".png";
            return new ImageIcon(IconRepository.class.getResource(path));
        } catch (Exception exception) {
            return new ImageIcon(IconRepository.class.getResource("default.png"));
        }
    }

    /**
     * Gets a default warning icon, in size LARGE (48x48).
     * 
     * @return the default warning icon
     */
    public final static ImageIcon getWarningIcon() {
        try {
            String path = LARGE + "/" + ACTIONS + "/messagebox_warning.png";
            return new ImageIcon(IconRepository.class.getResource(path));
        } catch (Exception exception) {
            return new ImageIcon(IconRepository.class.getResource("default.png"));
        }
    }

    /**
     * Gets a default critical / error icon, in size LARGE (48x48).
     * 
     * @return the default critical icon
     */
    public final static ImageIcon getCriticalIcon() {
        try {
            String path = LARGE + "/" + ACTIONS + "/messagebox_critical.png";
            return new ImageIcon(IconRepository.class.getResource(path));
        } catch (Exception exception) {
            return new ImageIcon(IconRepository.class.getResource("default.png"));
        }
    }

    /**
     * Gets a default information icon, in size LARGE (48x48).
     * 
     * @return the default information icon
     */
    public final static ImageIcon getInfoIcon() {
        try {
            String path = LARGE + "/" + ACTIONS + "/messagebox_info.png";
            return new ImageIcon(IconRepository.class.getResource(path));
        } catch (Exception exception) {
            return new ImageIcon(IconRepository.class.getResource("default.png"));
        }
    }

}
