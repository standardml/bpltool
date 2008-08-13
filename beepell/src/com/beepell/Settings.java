package com.beepell;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Logger;

/**
 * Singleton class to read settings.
 * TODO: Consider how to do this properly.
 * @author Tim Hallwyl
 */
public class Settings {

    private static Settings instance;

    private final Properties properties;

    private Logger log = Logger.getLogger("com.beepell.execution");
    
    /**
     * Get the singelton instance.
     * 
     * @return the singelton instance
     */
    public final static Settings getInstance() {
        if (instance == null)
            instance = new Settings();

        return instance;
    }

    private Settings() {
        properties = new Properties();

        try {
            properties.load(new FileInputStream("settings.txt"));
        } catch (IOException exception) {
            log.info("Settings file not found.");
        }
    }

    /**
     * 
     * @param name
     * @return the requested setting
     */
    public String getSetting(String name) {
        return properties.getProperty(name);
    }

    /**
     * @param name
     * @param defaultValue
     * @return the requested setting, or the default value
     */
    public String getSetting(String name, String defaultValue) {
        return properties.getProperty(name, defaultValue);
    }
    
    /**
     * Add or change a setting
     * @param name
     * @param value
     */
    public void setSetting(String name, String value) {
        properties.setProperty(name, value);
    }
}
