using System;
using System.Text;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSToProMeLaCompiler.FiniteRuns
{
    internal static class InlineEnabledActions
    {

        internal static void ClearEnabledActionsList(DCRSModel model, ref StringBuilder codeBuilder)
        {
            

            //inline clearenabledactions()
            //{
            //    index = 0;
            //    do
            //    :: index <  actioncount -> enabled_actions_set[index] = 0 ; index = index + 1;
            //    :: else ->	break;
            //    od;
            //}


            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append(string.Format("inline clear_enabled_actions() {0}", Environment.NewLine));

            codeBuilder.Append("{");

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append(string.Format("{0} index = 0; {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append(string.Format("{0} do {1}", Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} :: index <  actioncount -> enabled_actions_set[index] = 0 ; index = index + 1; {1}", Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} :: else -> break; {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append(string.Format("{0} od; {1}", Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append("}");

            codeBuilder.Append(Environment.NewLine);



        }


        internal static void ComputeEnabledActionsList(DCRSModel model, ref StringBuilder codeBuilder)
        {
            codeBuilder.Append(Environment.NewLine);




            // inline compute_enabled_actions()
            codeBuilder.Append(string.Format("inline compute_enabled_actions() {0}", Environment.NewLine));

            //{
            codeBuilder.Append("{");
            codeBuilder.Append(Environment.NewLine);

            //    index = 0;
            codeBuilder.Append(string.Format("{0} index = 0; {1}", Utilities.TAB,
                                             Environment.NewLine));

            //    /* Find out which actions are enabled */
            codeBuilder.Append(string.Format("{0} /* Find out which actions are enabled */ {1}", Utilities.TAB,
                                             Environment.NewLine));
            
            //    do /* Loop for outer dimesion, to loop row ount */
            codeBuilder.Append(string.Format("{0} do /* Loop for outer dimesion, to loop row ount */ {1}", Utilities.TAB,
                                             Environment.NewLine));

            //    :: index <  actioncount ->
            codeBuilder.Append(string.Format("{0} :: index <  actioncount -> {1}", Utilities.TAB,
                                             Environment.NewLine));

            //        if
            codeBuilder.Append(string.Format("{0}{0} if {1}", Utilities.TAB,
                                             Environment.NewLine));

            //        :: included_actions_set[index] == 1 ->
            codeBuilder.Append(string.Format("{0}{0} :: included_actions_set[index] == 1 -> {1}", Utilities.TAB,
                                             Environment.NewLine));

            //            index2 = 0;
            codeBuilder.Append(string.Format("{0}{0}{0} index2 = 0; {1}", Utilities.TAB,
                                             Environment.NewLine));

            //            can_execute = 1;		
            codeBuilder.Append(string.Format("{0}{0}{0} can_execute = 1; {1}", Utilities.TAB,
                                             Environment.NewLine));

            //            do /* inner loop for 2nd dimension */
            codeBuilder.Append(string.Format("{0}{0}{0} do /* inner loop for 2nd dimension */ {1}", Utilities.TAB,
                                             Environment.NewLine));

            //            :: index2 <  actioncount ->
            codeBuilder.Append(string.Format("{0}{0}{0} :: index2 <  actioncount -> {1}", Utilities.TAB,
                                             Environment.NewLine));

            //                if
            codeBuilder.Append(string.Format("{0}{0}{0}{0} if {1}", Utilities.TAB,
                                             Environment.NewLine));

            //                :: condition_relation[index].column[index2] == 1  ->
            codeBuilder.Append(string.Format("{0}{0}{0}{0} :: condition_relation[index].column[index2] == 1  -> {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));
            
            //                    if
            codeBuilder.Append(string.Format("{0}{0}{0}{0}{0} if {1}", Utilities.TAB,
                                             Environment.NewLine));

            // Rao: 2011-05-03: Changed to check if parent is included and not in executed set

            //                    :: included_actions_set[index2] == 1 && executed_actions_set[index2] != 1 -> can_execute = 0;
            codeBuilder.Append(
                string.Format("{0}{0}{0}{0}{0} :: included_actions_set[index2] == 1 && executed_actions_set[index2] != 1 -> can_execute = 0; {1}",
                              Utilities.TAB,
                              Environment.NewLine));

            //                    :: else ->skip;
            codeBuilder.Append(
                string.Format("{0}{0}{0}{0}{0} :: else ->skip; {1}",
                              Utilities.TAB,
                              Environment.NewLine));

            //                    fi;
            codeBuilder.Append(
                string.Format("{0}{0}{0}{0}{0} fi; {1}",
                              Utilities.TAB,
                              Environment.NewLine));

            
            //                :: else ->skip;  
            codeBuilder.Append(string.Format("{0}{0}{0}{0} :: else ->skip; {1}", Utilities.TAB,
                                             Environment.NewLine));


            //                fi;
            codeBuilder.Append(string.Format("{0}{0}{0}{0} fi; {1}", Utilities.TAB,
                                             Environment.NewLine));






            // Rao: 2011-05-03 Code for Milestones..check

                //// 2011:05:03 For Milestone relation check..
                // if 
                // :: milestone_relation[index].column[index2] == 1  -> 
                //     if 
                //     :: included_actions_set[index2] == 1 && pending_responses_set[index2] == 1 -> can_execute = 0; 
                //     :: else ->skip; 
                //     fi; 
                // :: else ->skip; 
                // fi; 



            
            //                if
            codeBuilder.Append(string.Format("{0}{0}{0}{0} if {1}", Utilities.TAB,
                                             Environment.NewLine));

            //                :: milestone_relation[index].column[index2] == 1  ->
            codeBuilder.Append(string.Format("{0}{0}{0}{0} :: milestone_relation[index].column[index2] == 1  -> {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //                    if
            codeBuilder.Append(string.Format("{0}{0}{0}{0}{0} if {1}", Utilities.TAB,
                                             Environment.NewLine));

            //                    :: pending_responses_set[index2] == 1 -> can_execute = 0;
            codeBuilder.Append(
                string.Format("{0}{0}{0}{0}{0} :: included_actions_set[index2] == 1 && pending_responses_set[index2] == 1 -> can_execute = 0; {1}",
                              Utilities.TAB,
                              Environment.NewLine));

            //                    :: else ->skip;
            codeBuilder.Append(
                string.Format("{0}{0}{0}{0}{0} :: else ->skip; {1}",
                              Utilities.TAB,
                              Environment.NewLine));

            //                    fi;
            codeBuilder.Append(
                string.Format("{0}{0}{0}{0}{0} fi; {1}",
                              Utilities.TAB,
                              Environment.NewLine));


            //                :: else ->skip;  
            codeBuilder.Append(string.Format("{0}{0}{0}{0} :: else ->skip; {1}", Utilities.TAB,
                                             Environment.NewLine));


            //                fi;
            codeBuilder.Append(string.Format("{0}{0}{0}{0} fi; {1}", Utilities.TAB,
                                             Environment.NewLine));










            //                index2 = index2 + 1;
            codeBuilder.Append(string.Format("{0}{0}{0}{0} index2 = index2 + 1; {1}", Utilities.TAB,
                                             Environment.NewLine));

            //            :: else ->	break; 
            codeBuilder.Append(string.Format("{0}{0}{0} :: else ->	break;  {1}", Utilities.TAB,
                                             Environment.NewLine));


            //            od;
            codeBuilder.Append(string.Format("{0}{0}{0} od; {1}", Utilities.TAB,
                                             Environment.NewLine));




            // 2010-02-20:Removed the following code as more optimised code is replaced here
            ////            if
            //codeBuilder.Append(string.Format("{0}{0}{0} if {1}", Utilities.TAB,
            //                                 Environment.NewLine));


            ////            :: can_execute == 1 -> 	enabled_actions_set[index] = 1;
            //codeBuilder.Append(string.Format("{0}{0}{0} :: can_execute == 1 -> 	enabled_actions_set[index] = 1; {1}", Utilities.TAB,
            //                                 Environment.NewLine));

            ////            :: else -> skip;	
            //codeBuilder.Append(string.Format("{0}{0}{0} :: else -> skip; {1}", Utilities.TAB,
            //                                 Environment.NewLine));
            
            ////            fi;
            //codeBuilder.Append(string.Format("{0}{0}{0} fi; {1}", Utilities.TAB,
            //                                 Environment.NewLine));



            // 2010-02-20: replaced code..
            //enabled_actions_set[index] = (can_execute -> 1 : 0);
            codeBuilder.Append(string.Format("{0}{0}{0} enabled_actions_set[index] = (can_execute -> 1 : 0); {1}", Utilities.TAB,
                                             Environment.NewLine));
            

            
            //        ::else -> skip;
            codeBuilder.Append(string.Format("{0}{0} ::else -> skip; {1}", Utilities.TAB,
                                             Environment.NewLine));
            
            //        fi;
            codeBuilder.Append(string.Format("{0}{0} fi; {1}", Utilities.TAB,
                                             Environment.NewLine));


            //        index++;
            codeBuilder.Append(string.Format("{0}{0} index++; {1}", Utilities.TAB,
                                             Environment.NewLine));
            
            //    :: else ->	break;
            codeBuilder.Append(string.Format("{0} :: else ->	break; {1}", Utilities.TAB,
                                             Environment.NewLine));

            
            //    od;
            codeBuilder.Append(string.Format("{0} od; {1}", Utilities.TAB,
                                             Environment.NewLine));


            //}
            codeBuilder.Append("}");

            codeBuilder.Append(Environment.NewLine);


        }




    }
}