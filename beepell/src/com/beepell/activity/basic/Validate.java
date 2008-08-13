package com.beepell.activity.basic;

import java.util.List;

import com.beepell.activity.ActivityState;
import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.model.ValidateActivity;

/**
 * The validate activity can be used to ensure that values of variables are
 * valid against their associated XML data definition, including XML Schema
 * simple type, complex type, element definition and XML definitions of WSDL
 * parts. The validate activity has a variables attribute, listing the variables
 * to validate. [8.1]
 * 
 * @author Tim Hallwyl
 */
public class Validate extends AbstractBasicActivity {

    private final List<String> variables;

    /**
     * Creates an instance of the Validate activity.
     * 
     * @param configuration
     */
    public Validate(ValidateActivity configuration) {
        super(configuration);
        variables = configuration.getVariables();
        setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() throws InvalidVariables, UninitializedVariable {
        for (String variable : variables) {

            if (getState() == ActivityState.TERMINATING)
                return;

            context.validate(variable);
        }
    }

}
