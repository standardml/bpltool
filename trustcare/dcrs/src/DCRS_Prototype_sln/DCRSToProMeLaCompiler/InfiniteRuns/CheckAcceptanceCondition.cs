using System;
using System.Text;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSToProMeLaCompiler.InfiniteRuns
{
    class CheckAcceptanceCondition
    {

        internal static void WriteInlinecheckStateAcceptanceCondition(DCRSModel model, ref StringBuilder codeBuilder)
        {

            codeBuilder.Append(Environment.NewLine);



            //inline check_state_acceptance_condition()
            //{
            codeBuilder.Append(string.Format("inline check_state_acceptance_condition() {0}", Environment.NewLine));

            codeBuilder.Append("{");

            codeBuilder.Append(Environment.NewLine);

            
            //    if
            codeBuilder.Append(string.Format("{0} if {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        /* If no pending responses in the next set. */
            codeBuilder.Append(string.Format("{0}{0} /* If no pending responses in the next set. */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        :: (include_response_nextstate_set_count == 0) ->
            codeBuilder.Append(string.Format("{0}{0} :: (include_response_nextstate_set_count == 0) -> {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //accept_state_0: accepting_state_visited = 1;
            codeBuilder.Append(string.Format("{0}{0} accept_state_0: accepting_state_visited = 1; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));


            //        :: ((m_set_count > 0) && (acceptable_responses_set[min_m_set])) ->
            codeBuilder.Append(string.Format("{0}{0} :: ((m_set_count > 0) && (acceptable_responses_set[min_m_set])) -> {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //accept_state_1: accepting_state_visited = 1; state_index = min_m_set ;
            codeBuilder.Append(string.Format("{0}{0} accept_state_1: accepting_state_visited = 1; state_index = min_m_set ; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));


            //        :: ((m_set_count == 0) && (min_include_response_current < actioncount) && (acceptable_responses_set[min_include_response_current])) ->
            codeBuilder.Append(string.Format("{0}{0} :: ((m_set_count == 0) && (min_include_response_current < actioncount) && (acceptable_responses_set[min_include_response_current])) -> {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //accept_state_2: accepting_state_visited = 1; state_index = min_include_response_current ;
            codeBuilder.Append(string.Format("{0}{0} accept_state_2: accepting_state_visited = 1; state_index = min_include_response_current ; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //            /* Otherwise dont change the state index. */
            codeBuilder.Append(string.Format("{0}{0} /* Otherwise dont change the state index. */ {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //        :: else -> accepting_state_visited = 0; 
            codeBuilder.Append(string.Format("{0}{0} :: else -> accepting_state_visited = 0;  {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    fi;
            codeBuilder.Append(string.Format("{0} fi; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //}

            codeBuilder.Append("}");

            codeBuilder.Append(Environment.NewLine);

            



        }


    }
}
