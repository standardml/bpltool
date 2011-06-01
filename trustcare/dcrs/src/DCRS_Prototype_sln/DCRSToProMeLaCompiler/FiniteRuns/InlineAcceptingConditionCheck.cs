using System;
using System.Text;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSToProMeLaCompiler.FiniteRuns
{
    internal static class InlineAcceptingConditionCheck
    {


        internal static void WriteAcceptingConditionOverFiniteTrace(DCRSModel model, ref StringBuilder codeBuilder)
        {

            //inline accepting_state_check()
            //{
            //    /* Find out whether there are 0 responses */		
            //    index = 0;
            //    state_accepted = 1;
            //    do
            //    :: index <  action_count ->
            //        if
            //        :: pending_responses_set[index] == 1 -> state_accepted = 0 ;
            //        :: else -> skip;
            //        fi;
            //        index = index + 1;
            //    :: else -> break;  
            //    od;
            //}


            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append(string.Format("inline accepting_state_check() {0}", Environment.NewLine));

            codeBuilder.Append("{");

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append(string.Format("{0} /* Find out whether there are 0 responses */	 {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} index = 0; {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append(string.Format("{0} state_accepted = 1; {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append(string.Format("{0} do {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append(string.Format("{0} :: index <  action_count -> {1}", Utilities.TAB,
                                             Environment.NewLine));



            codeBuilder.Append(string.Format("{0}{0} if {1}", Utilities.TAB,
                                             Environment.NewLine));

            // Rao: 2011.06.01: Check for included pending responses.
            codeBuilder.Append(string.Format("{0}{0} :: (pending_responses_set[index] == 1) && (included_actions_set[index] == 1) -> state_accepted = 0 ; {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append(string.Format("{0}{0} :: else -> skip; {1}", Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} fi; {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append(string.Format("{0}{0} index = index + 1; {1}", Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} :: else -> break;  {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append(string.Format("{0} od;  {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append("}");

            codeBuilder.Append(Environment.NewLine);





        }



    }
}