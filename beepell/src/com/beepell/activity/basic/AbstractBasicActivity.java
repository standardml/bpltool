package com.beepell.activity.basic;

import com.beepell.activity.AbstractActivity;
import com.beepell.model.Activity;

/**
 * @author Tim Hallwyl
 */
public abstract class AbstractBasicActivity extends AbstractActivity {

    /**
     * Creates the activity based on the configuration.
     * @param configuration
     */
    public AbstractBasicActivity(Activity configuration) {
        super(configuration);

    }
}
