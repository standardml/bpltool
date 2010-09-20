using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSToProMeLaCompiler.FiniteRuns
{
    internal static class DcrsPromelaProctype
    {


        internal static void WriteDCRSProcessDeclarationForFiniteComputation(DCRSModel model, ref StringBuilder codeBuilder)
        {
            codeBuilder.Append(Environment.NewLine);

            //active proctype dcrs()
            //{
            codeBuilder.Append(string.Format("active proctype dcrs() {0}", Environment.NewLine));

            codeBuilder.Append("{");

            codeBuilder.Append(Environment.NewLine);


            //    /* Call  model_specification() to assign necessary constraints*/
            //    model_specification();

            codeBuilder.Append(string.Format("{0} /* Call  model_specification() to assign necessary constraints*/ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} model_specification(); {1}", Utilities.TAB,
                                             Environment.NewLine));
            //    do 
            //    :: 

            codeBuilder.Append(string.Format("{0} do {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} :: {1}", Utilities.TAB,
                                             Environment.NewLine));



            //        /* Clearing away enabled_actions_set set */
            //        clear_enabled_actions();
            codeBuilder.Append(string.Format("{0}{0} /* Clearing away enabled_actions_set set */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} clear_enabled_actions(); {1}", Utilities.TAB,
                                             Environment.NewLine));

            //        /* Compute which ations are enabled based on latest execution set */
            //        compute_enabled_actions();
            codeBuilder.Append(string.Format("{0}{0} /* Compute which ations are enabled based on latest execution set */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} compute_enabled_actions(); {1}", Utilities.TAB,
                                             Environment.NewLine));



            //        /* Execute an action non-nondeterministically */
            //        nondeterministic_execution();
            codeBuilder.Append(string.Format("{0}{0} /* Execute an action non-nondeterministically */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} nondeterministic_execution(); {1}", Utilities.TAB,
                                             Environment.NewLine));



            //        /* Compute state after execution. */
            //        compute_state_after_execution();
            codeBuilder.Append(string.Format("{0}{0} /* Compute state after execution. */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} compute_state_after_execution(); {1}", Utilities.TAB,
                                             Environment.NewLine));


            //        /* Check whether accepting state is reached */
            //        accepting_state_check();
            codeBuilder.Append(string.Format("{0}{0} /* Check whether accepting state is reached */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} accepting_state_check(); {1}", Utilities.TAB,
                                             Environment.NewLine));


            //        if
            //        :: state_accepted == 1 -> accepted_state_reached = 1; goto  accepting_state;
            //        ::  accepted_state_reached = 0; 
            //        fi;

            codeBuilder.Append(string.Format("{0}{0} if {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} :: state_accepted == 1 -> accepted_state_reached = 1; goto  accepting_state; {1}", Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} ::  accepted_state_reached = 0;  {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} fi; {1}", Utilities.TAB,
                                             Environment.NewLine));





            //    od;

            //    accepting_state: printf("accepting state reached after %u", actions_executed_count);
            //}

            codeBuilder.Append(string.Format("{0} od; {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append(
                string.Format(
                    "{0} accepting_state: printf(\"accepting state reached after %u\", actions_executed_count); {1}",
                    Utilities.TAB,
                    Environment.NewLine));

            codeBuilder.Append("}");

            codeBuilder.Append(Environment.NewLine);









        }


        internal static void WriteDCRSProcessDeclarationForInFiniteRuns(DCRSModel model, ref StringBuilder codeBuilder)
        {


            codeBuilder.Append(Environment.NewLine);

            //active proctype dcrs()
            //{
            codeBuilder.Append(string.Format("active proctype dcrs() {0}", Environment.NewLine));

            codeBuilder.Append("{");

            codeBuilder.Append(Environment.NewLine);


            //    /* Call  model_specification() to assign necessary constraints*/
            //    model_specification();

            codeBuilder.Append(string.Format("{0} /* Call  model_specification() to assign necessary constraints*/ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} model_specification(); {1}", Utilities.TAB,
                                             Environment.NewLine));
            //    do 
            //    :: 

            codeBuilder.Append(string.Format("{0} do {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} :: {1}", Utilities.TAB,
                                             Environment.NewLine));


            //        /* Compute which ations are enabled based on latest execution set */
            //        compute_enabled_actions();
            codeBuilder.Append(string.Format("{0}{0} /* Compute which ations are enabled based on latest execution set */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} compute_enabled_actions(); {1}", Utilities.TAB,
                                             Environment.NewLine));



            //        /* Execute an action non-nondeterministically */
            //        nondeterministic_execution();
            codeBuilder.Append(string.Format("{0}{0} /* Execute an action non-nondeterministically */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} nondeterministic_execution(); {1}", Utilities.TAB,
                                             Environment.NewLine));



            //        /* Compute include response sets and m-set etc */
            //        compute_include_response_sets();
            codeBuilder.Append(string.Format("{0}{0} /* Compute include response sets and m-set etc */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} compute_include_response_sets(); {1}", Utilities.TAB,
                                             Environment.NewLine));

            //        /* Compute minimum values for include response sets and m-set etc */
            //        compute_set_minimum();
            codeBuilder.Append(string.Format("{0}{0} /* Compute minimum values for include response sets and m-set etc */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} compute_set_minimum(); {1}", Utilities.TAB,
                                             Environment.NewLine));


            //        /* Compute state accepting conditions */
            //        check_state_acceptance_condition();
            codeBuilder.Append(string.Format("{0}{0} /* Compute state accepting conditions  */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} check_state_acceptance_condition(); {1}", Utilities.TAB,
                                             Environment.NewLine));


            //        /* Compute state after execution. */
            //        compute_state_after_execution();
            codeBuilder.Append(string.Format("{0}{0} /* Compute state after execution. */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} compute_state_after_execution(); {1}", Utilities.TAB,
                                             Environment.NewLine));


            //printf("accepting state reached after %u", actions_executed_count);

            codeBuilder.Append(string.Format("{0}{0} printf(\"accepting state reached after %u\", actions_executed_count); {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append(string.Format("{0} od; {1}", Utilities.TAB,
                                 Environment.NewLine));


            codeBuilder.Append("}");

            codeBuilder.Append(Environment.NewLine);



        }




    }
}