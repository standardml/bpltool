using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSToProMeLaCompiler.FiniteRuns
{
    internal static class InlineNonDeterministicExecution
    {


        internal static void WriteNonDeterministicExecution(DCRSModel model, ref StringBuilder codeBuilder)
        {
            //inline nondeterministic_execution()
            //{
            
                //// Rao: 2011-06-03: Code for checking whether any included pending responses..
                //any_included_pending_responses = 0;

                // index = 0; 

                // do 
                // :: index <  action_count -> 
                //     if 
                //     :: (pending_responses_set[index] == 1 ) && (included_actions_set[index] == 1) -> any_included_pending_responses = 1 ; 
                //     :: else -> skip; 
                //     fi; 
                //     index = index + 1; 
                // :: else -> break;  
                // od;            
            
            
            //    /* Dynamic non deterministic execution condition. */
            //    if
            //    :: (enabled_actions_set[OM] == 1) -> random_action_executed = OM;
            //    :: (enabled_actions_set[S] == 1)  -> random_action_executed = S;
            //    :: (enabled_actions_set[GM] == 1) -> random_action_executed = GM;
            //    :: else -> 
            //    dead_lock_reached:	 assert(false);
            //    fi;
            //}

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append(string.Format("inline nondeterministic_execution() {0}", Environment.NewLine));

            codeBuilder.Append("{");

            codeBuilder.Append(Environment.NewLine);


            codeBuilder.Append(string.Format("{0} any_included_pending_responses = 0; {1}", Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} index = 0; {1}", Utilities.TAB,
                                             Environment.NewLine));
            

            // do 
            codeBuilder.Append(string.Format("{0} do {1}", Utilities.TAB,
                                             Environment.NewLine));
            
            // :: index <  action_count -> 
            codeBuilder.Append(string.Format("{0}{0} :: index <  action_count -> {1}", Utilities.TAB,
                                             Environment.NewLine));
            
            //     if 
            codeBuilder.Append(string.Format("{0}{0}{0} if {1}", Utilities.TAB,
                                             Environment.NewLine));
            
            //     :: (pending_responses_set[index] == 1 ) && (included_actions_set[index] == 1) -> any_included_pending_responses = 1 ;
            codeBuilder.Append(string.Format("{0}{0}{0} :: (pending_responses_set[index] == 1 ) && (included_actions_set[index] == 1) -> any_included_pending_responses = 1 ; {1}", Utilities.TAB,
                                                        Environment.NewLine));
            

            //     :: else -> skip; 
            codeBuilder.Append(string.Format("{0}{0}{0} :: else -> skip;  {1}", Utilities.TAB,
                                                         Environment.NewLine));

            //     fi; 
            codeBuilder.Append(string.Format("{0}{0}{0} fi; {1}", Utilities.TAB,
                                             Environment.NewLine));

            //     index = index + 1; 
            codeBuilder.Append(string.Format("{0}{0} index = index + 1; {1}", Utilities.TAB,
                                             Environment.NewLine));

            // :: else -> break;  
            codeBuilder.Append(string.Format("{0}{0} :: else -> break; {1}", Utilities.TAB,
                                             Environment.NewLine));

            // od;  
            codeBuilder.Append(string.Format("{0} od; {1}", Utilities.TAB,
                                             Environment.NewLine));

                      

            

            codeBuilder.Append(string.Format("{0} /* Dynamic non deterministic execution condition. */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} if {1}", Utilities.TAB,
                                             Environment.NewLine));


            foreach (KeyValuePair<short, string> keyValuePair in model.ActionList)
            {

                //    :: (enabled_actions_set[OM] == 1) -> random_action_executed = OM;

                codeBuilder.Append(
                    string.Format("{0} :: (enabled_actions_set[{1}] == 1) -> random_action_executed = {1}; {2}",
                                  Utilities.TAB, keyValuePair.Value,
                                  Environment.NewLine));
            }



             //:: else -> 
             //       if
             //           :: (any_included_pending_responses) -> dead_lock_reached:	assert(false);
             //           /* If we dont have any actions enabled and no included pending responses, then we exit. */
             //           :: else -> goto accepting_state;
             //       fi;


            codeBuilder.Append(string.Format("{0} :: else -> {1}", Utilities.TAB,
                                             Environment.NewLine));


            //       if
            codeBuilder.Append(string.Format("{0}{0} if {1}", Utilities.TAB,
                                             Environment.NewLine));
            //           :: (any_included_pending_responses) -> 
 
            codeBuilder.Append(string.Format("{0}{0} :: (any_included_pending_responses) -> {1}", Utilities.TAB,
                                             Environment.NewLine));

            // dead_lock_reached:	printf("accepting state reached after %u", actions_executed_count);  assert(false);

            codeBuilder.Append(
                string.Format(
                    "{0}{0} dead_lock_reached:	printf(\"Dead lock reached after %u\", actions_executed_count); assert(false); {1}",
                    Utilities.TAB,
                    Environment.NewLine));


            //           /* If we dont have any actions enabled and no included pending responses, then we exit. */
            codeBuilder.Append(string.Format("{0}{0} /* If we dont have any actions enabled and no included pending responses, then we exit. */ {1}", Utilities.TAB,
                                             Environment.NewLine));
            //           :: else -> goto accepting_state;
            codeBuilder.Append(string.Format("{0}{0} :: else -> goto accepting_state; {1}", Utilities.TAB,
                                             Environment.NewLine));
            //       fi;
            codeBuilder.Append(string.Format("{0}{0} fi; {1}", Utilities.TAB,
                                             Environment.NewLine));
          
            
            //codeBuilder.Append(string.Format("{0} dead_lock_reached:	assert(false); {1}", Utilities.TAB,
            //                                 Environment.NewLine));


            codeBuilder.Append(string.Format("{0} fi; {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append("}");

            codeBuilder.Append(Environment.NewLine);









        }

    }
}