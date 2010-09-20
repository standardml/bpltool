using System;
using System.Text;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSToProMeLaCompiler
{
    internal static class InlineStateComputation
    {



        internal static void WriteStateComputationAfterExecution(DCRSModel model, ref StringBuilder codeBuilder)
        {


            codeBuilder.Append(Environment.NewLine);

            //inline compute_state_after_execution()
            //{

            codeBuilder.Append(string.Format("inline compute_state_after_execution() {0}", Environment.NewLine));

            codeBuilder.Append("{");

            codeBuilder.Append(Environment.NewLine);


            //    /* Update executed actions set*/
            //    executed_actions_set[random_action_executed] = 1 ;

            codeBuilder.Append(string.Format("{0} /* Update executed actions set*/	 {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} executed_actions_set[random_action_executed] = 1 ; {1}", Utilities.TAB,
                                             Environment.NewLine));

            //    actions_executed_count++;

            //    /* Delete entry from pending_responses_set set if it is a response to some other action*/
            //    pending_responses_set[random_action_executed] = 0 ;

            codeBuilder.Append(string.Format("{0} actions_executed_count++;	 {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} /* Delete entry from pending_responses_set set if it is a response to some other action*/ {1}", Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} pending_responses_set[random_action_executed] = 0 ;	 {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    index = 0;
            codeBuilder.Append(string.Format("{0} index = 0; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    do
            codeBuilder.Append(string.Format("{0} do {1}",
                                 Utilities.TAB,
                                 Environment.NewLine));
           
            //    :: index <  actioncount ->
            codeBuilder.Append(string.Format("{0} :: index <  actioncount -> {1}",
                                 Utilities.TAB,
                                 Environment.NewLine));



            // /* Include actions which are included by this action in set included_actions_set */
            //if
            //:: include_relation[random_action_executed].column[index] == 1 -> included_actions_set[index] = 1 ;
            //:: else -> skip;
            //fi;


            UpdateRelationSet("/* Include actions which are included by this action in set included_actions_set */",
                              ":: include_relation[random_action_executed].column[index] == 1 -> included_actions_set[index] = 1 ;",
                              codeBuilder);


            ///* Exclude actions which are excluded by this action in set included_actions_set */
            //if
            //:: exclude_relation[random_action_executed].column[index] == 1 -> included_actions_set[index] = 0 ;
            //:: else -> skip;
            //fi;

            UpdateRelationSet("/* Exclude actions which are excluded by this action in set included_actions_set */",
                              ":: exclude_relation[random_action_executed].column[index] == 1 -> included_actions_set[index] = 0 ;",
                              codeBuilder);


            ///* Include actions which are responses to this action in set pending_responses_set */
            //if
            //:: response_relation[random_action_executed].column[index] == 1 -> pending_responses_set[index] = 1 ;
            //:: else -> skip;
            //fi;

            UpdateRelationSet("/* Include actions which are responses to this action in set pending_responses_set */",
                              ":: response_relation[random_action_executed].column[index] == 1 -> pending_responses_set[index] = 1 ;",
                              codeBuilder);

            

            codeBuilder.Append(Environment.NewLine);


            //        index = index + 1;
            //    :: else -> break;  
            //    od;

            //}

            codeBuilder.Append(string.Format("{0}{0} index = index + 1; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append(string.Format("{0} :: else -> break; {1}",
                                 Utilities.TAB,
                                 Environment.NewLine));

            codeBuilder.Append(string.Format("{0} od; {1}",
                                 Utilities.TAB,
                                 Environment.NewLine));


            codeBuilder.Append("}");

            codeBuilder.Append(Environment.NewLine);


        }



        private static void UpdateRelationSet(string comment, string instruction, StringBuilder codeBuilder)
        {


            //        /* Include actions which are responses to this action in set pending_responses_set */
            //        if
            //        :: response_relation[random_action_executed].column[index] == 1 -> pending_responses_set[index] = 1 ;
            //        :: else -> skip;
            //        fi;


            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append(
                string.Format(
                    "{0}{0} {2}	 {1}",
                    Utilities.TAB,
                    Environment.NewLine, comment));

            codeBuilder.Append(string.Format("{0}{0} if {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} {2} {1}",
                                 Utilities.TAB,
                                 Environment.NewLine, instruction));

            codeBuilder.Append(string.Format("{0}{0} :: else -> skip; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0}{0} fi; {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));





        }



    }
}
