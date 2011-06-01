using System;
using System.Text;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSToProMeLaCompiler.InfiniteRuns
{
    class InlineComputeResponseSets
    {



        internal static void WriteInlineComputeIncludeResponseSets(DCRSModel model, ref StringBuilder codeBuilder)
        {

            codeBuilder.Append(Environment.NewLine);

            //inline compute_include_response_sets()
            //{

            codeBuilder.Append(string.Format("inline compute_include_response_sets() {0}", Environment.NewLine));

            codeBuilder.Append("{");

            codeBuilder.Append(Environment.NewLine);


            //    index = 0;
            codeBuilder.Append(string.Format("{0} index = 0; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    do
            codeBuilder.Append(string.Format("{0} do {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    :: index <  action_count ->
            codeBuilder.Append(string.Format("{0} :: index <  action_count -> {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));


            //        /* Update for include_response_current set. */
            codeBuilder.Append(string.Format("{0}{0} /* Update for include_response_current set. */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        include_response_current[index] = ( (included_actions_set[index] && pending_responses_set[index]) -> 1: 0 );
            codeBuilder.Append(string.Format("{0}{0} include_response_current[index] = ( (included_actions_set[index] && pending_responses_set[index]) -> 1: 0 ); {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        /* Calculation of next state set */
            codeBuilder.Append(string.Format("{0}{0} /* Calculation of next state set */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        /* Updating the included_actions_nextstate set */
            codeBuilder.Append(string.Format("{0}{0} /* Updating the included_actions_nextstate set */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        if
            codeBuilder.Append(string.Format("{0}{0} if {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //            :: include_relation[random_action_executed].column[index] -> included_actions_nextstate[index] = 1 ;
            codeBuilder.Append(string.Format("{0}{0} :: include_relation[random_action_executed].column[index] -> included_actions_nextstate[index] = 1 ; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //            :: exclude_relation[random_action_executed].column[index] -> included_actions_nextstate[index] = 0 ;
            codeBuilder.Append(string.Format("{0}{0} :: exclude_relation[random_action_executed].column[index] -> included_actions_nextstate[index] = 0 ; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //            :: else -> included_actions_nextstate[index] = included_actions_set[index];
            codeBuilder.Append(string.Format("{0}{0} :: else -> included_actions_nextstate[index] = included_actions_set[index]; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        fi;
            codeBuilder.Append(string.Format("{0}{0} fi; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        /* Updating the pending_responses_nextstate set */
            //        /* Clear the  pending response for random_action_executed unless it is not included by itself */
            codeBuilder.Append(string.Format("{0}{0} /* Updating the pending_responses_nextstate set */ {1} {0}{0} /* Clear the  pending response for random_action_executed unless it is not included by itself */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        if
            codeBuilder.Append(string.Format("{0}{0} if {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //            :: response_relation[random_action_executed].column[index] -> pending_responses_nextstate[index] = 1 ;
            codeBuilder.Append(string.Format("{0}{0} :: response_relation[random_action_executed].column[index] -> pending_responses_nextstate[index] = 1 ; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        :: else -> pending_responses_nextstate[index] = ((random_action_executed == index) -> 0: pending_responses_set[index]);
            codeBuilder.Append(string.Format("{0}{0} :: else -> pending_responses_nextstate[index] = ((random_action_executed == index) -> 0: pending_responses_set[index]); {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        fi;
            codeBuilder.Append(string.Format("{0}{0} fi; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        /* Updating the include_response_nextstate set */
            codeBuilder.Append(string.Format("{0}{0} /* Updating the include_response_nextstate set */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        include_response_nextstate[index] = ( (included_actions_nextstate[index] && pending_responses_nextstate[index]) -> 1: 0 );
            codeBuilder.Append(string.Format("{0}{0} include_response_nextstate[index] = ( (included_actions_nextstate[index] && pending_responses_nextstate[index]) -> 1: 0 ); {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));


            //        /* Compute the acceptable_responses_set (I and R \ (I' and R') U {e})  */
            codeBuilder.Append(string.Format("{0}{0} /* Compute the acceptable_responses_set (I and R \\ (I' and R') U (e)) */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        acceptable_responses_set[index] = ( include_response_current[index]  && (!include_response_nextstate[index]) -> 1:0 );
            codeBuilder.Append(string.Format("{0}{0} acceptable_responses_set[index] = ( include_response_current[index]  && (!include_response_nextstate[index]) -> 1:0 ); {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));


            //        m_set[index] = ((include_response_current[index] && (index > state_index)) -> 1: 0);
            codeBuilder.Append(string.Format("{0}{0} m_set[index] = ((include_response_current[index] && (index > state_index)) -> 1: 0); {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        index = index + 1;
            codeBuilder.Append(string.Format("{0}{0} index = index + 1; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    :: else ->	break;
            codeBuilder.Append(string.Format("{0} :: else -> break; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    od;
            codeBuilder.Append(string.Format("{0} od; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    /* Add the current random action executed to the acceptable_responses_set to get (I and R \ (I' and R') U {e})*/
            codeBuilder.Append(string.Format("{0} /* Add the current random action executed to the acceptable_responses_set to get (I and R \\ (I' and R') U (e))*/ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    acceptable_responses_set[random_action_executed] = 1;
            codeBuilder.Append(string.Format("{0} acceptable_responses_set[random_action_executed] = 1; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));


            //}
            codeBuilder.Append("}");

            codeBuilder.Append(Environment.NewLine);

        }



        internal static void WriteInlineMinimunIncludeResponseSets(DCRSModel model, ref StringBuilder codeBuilder)
        {
            codeBuilder.Append(Environment.NewLine);

            //inline compute_set_minimum()
            //{
            codeBuilder.Append(string.Format("inline compute_set_minimum() {0}", Environment.NewLine));

            codeBuilder.Append("{");

            codeBuilder.Append(Environment.NewLine);

            
            //    /* Initially set the min_m_set to highest number as default as 0 is also used as action index */
            codeBuilder.Append(string.Format("{0} /* Initially set the min_m_set to highest number as default as 0 is also used as action index */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    min_m_set = action_count;
            codeBuilder.Append(string.Format("{0} min_m_set = action_count; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));


            //    min_include_response_current = action_count;
            codeBuilder.Append(string.Format("{0} min_include_response_current = action_count; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    /* Assign the index to action_count, as we will loop through the array in reverse order to find out min. */
            codeBuilder.Append(string.Format("{0} /* Assign the index to action_count, as we will loop through the array in reverse order to find out min. */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    index = action_count;
            codeBuilder.Append(string.Format("{0} index = action_count; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));
            //m_set_count = 0;
            codeBuilder.Append(string.Format("{0} m_set_count = 0; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));




            //include_response_current_set_count = 0;
            codeBuilder.Append(string.Format("{0} include_response_current_set_count = 0; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));


            //include_response_nextstate_set_count = 0;
            codeBuilder.Append(string.Format("{0} include_response_nextstate_set_count = 0; {1}",
                                 Utilities.TAB,
                                 Environment.NewLine));



            //    do
            codeBuilder.Append(string.Format("{0} do {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    :: index > 0 ->
            codeBuilder.Append(string.Format("{0} :: index > 0 -> {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        /* min for m_set */
            codeBuilder.Append(string.Format("{0}{0} /* min for m_set */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        if
            codeBuilder.Append(string.Format("{0}{0} if {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        :: m_set[index -1] -> min_m_set = (index -1);
            codeBuilder.Append(string.Format("{0}{0} :: m_set[index -1] -> min_m_set = (index -1); {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //            m_set_count++;
            codeBuilder.Append(string.Format("{0}{0}{0} m_set_count++; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        :: else -> skip;
            codeBuilder.Append(string.Format("{0}{0} :: else -> skip; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        fi;
            codeBuilder.Append(string.Format("{0}{0} fi; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        /* min for include_response_current set */
            codeBuilder.Append(string.Format("{0}{0} /* min for include_response_current set */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        if
            codeBuilder.Append(string.Format("{0}{0} if {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        :: include_response_current[index -1] -> min_include_response_current = (index -1);
            codeBuilder.Append(string.Format("{0}{0} :: include_response_current[index -1] -> min_include_response_current = (index -1); {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //            include_response_current_set_count++;
            codeBuilder.Append(string.Format("{0}{0}{0} include_response_current_set_count++; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        :: else -> skip;
            codeBuilder.Append(string.Format("{0}{0} :: else -> skip; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        fi;
            codeBuilder.Append(string.Format("{0}{0} fi; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        /* Find out how many elements are in the include_response_nextstate set */
            codeBuilder.Append(string.Format("{0}{0} /* Find out how many elements are in the include_response_nextstate set */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        include_response_nextstate_set_count = (include_response_nextstate[index -1] -> include_response_nextstate_set_count + 1 : include_response_nextstate_set_count); 
            codeBuilder.Append(string.Format("{0}{0} include_response_nextstate_set_count = (include_response_nextstate[index -1] -> include_response_nextstate_set_count + 1 : include_response_nextstate_set_count);  {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));


            //        index--;
            codeBuilder.Append(string.Format("{0}{0} index--; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    :: else -> break;
            codeBuilder.Append(string.Format("{0} :: else -> break; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    od;
            codeBuilder.Append(string.Format("{0} od; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //}

            codeBuilder.Append("}");

            codeBuilder.Append(Environment.NewLine);
            

        }










    }
}
