using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSToProMeLaCompiler
{
    internal static class InlineSpecification
    {

        //inline model_specification()
        //{
        //    /* actions set + included/excluded */
        //    actions_array[OM].actionid = OM;
        //    actions_array[OM].included = 1;

        //    actions_array[S].actionid = S;
        //    actions_array[S].included = 1;

        //    actions_array[GM].actionid = GM;
        //    actions_array[GM].included = 1;

        //    condition_relation[S].column[OM] = 1;
        //    condition_relation[GM].column[S] = 1;

        //    response_relation[OM].column[S] = 1;
        //    response_relation[OM].column[GM] = 1;

        //    /* Prepare the initial state by copying all the actions with included bit to included_actions_set */
        //    do
        //    :: index <  actioncount ->
        //        included_actions_set[index] = (actions_array[index].included == 1 -> 1: 0);
        //        index = index + 1;
        //    :: else -> 	break;
        //    od;
        //}


        internal static void WriteModelSpecification(DCRSModel model, ref StringBuilder codeBuilder)
        {
            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append("inline model_specification()");

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append("{"); 
            
            codeBuilder.Append(Environment.NewLine); 
            

            FillActionsArray(model, codeBuilder);

           
            WriteRelations(model, codeBuilder, model.Conditions, "condition_relation");

            WriteRelations(model, codeBuilder, model.MileStones, "milestone_relation");

            WriteRelations(model, codeBuilder, model.Responses, "response_relation");


            WriteRelations(model, codeBuilder, model.Includes, "include_relation");

            WriteRelations(model, codeBuilder, model.Excludes, "exclude_relation");


            CopyInitialState(codeBuilder);


            codeBuilder.Append("}");

            codeBuilder.Append(Environment.NewLine);


        }






        #region Helper Methods.

        private static void FillActionsArray(DCRSModel model, StringBuilder codeBuilder)
        {



            foreach (KeyValuePair<short, string> keyValuePair in model.ActionList)
            {
                //actions_array[OM].actionid = OM;
                codeBuilder.Append(string.Format("{0} actions_array[{1}].actionid = {1}; {2}", Utilities.TAB,
                                                 keyValuePair.Value, Environment.NewLine));

                // Get the included value from the initial state.
                var includedbit = Utilities.GetValueFromTwoDimArray(model.InitialState, keyValuePair.Key);

                // Here use the default value as 1 for included bit that means, if an action is not 
                // specified explicitly excluded, we treat the default as included.
                var included = (includedbit.HasValue) && (includedbit == 0) ? 0 : 1;

                //    actions_array[OM].included = 1;
                codeBuilder.Append(string.Format("{0} actions_array[{1}].included = {2}; {3}", Utilities.TAB,
                                                 keyValuePair.Value, included, Environment.NewLine));
            }



        }

        private static void WriteRelations(DCRSModel model, StringBuilder codeBuilder, 
            short[,] relationArray, string relationName)
        {

            //    condition_relation[S].column[OM] = 1;

            codeBuilder.Append(Environment.NewLine);

            for (var index = 0; index < relationArray.GetLength(0); index++)
            {
                var relationSource = model.ActionList[relationArray[index, 0]];

                var relationTarget = model.ActionList[relationArray[index, 1]];


                //actions_array[OM].actionid = OM;
                codeBuilder.Append(string.Format("{0} {1}[{2}].column[{3}] = 1; {4}", Utilities.TAB,
                                                 relationName, relationSource, relationTarget, Environment.NewLine));


            }


            


        }


        private static void CopyInitialState(StringBuilder codeBuilder)
        {

            //do
            //:: index <  actioncount ->
            //    included_actions_set[index] = (actions_array[index].included == 1 -> 1: 0);
            //    index = index + 1;
            //:: else -> 	break;
            //od;

            
            
            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append(string.Format("{0} /* Prepare the initial state */ {1}", Utilities.TAB,
                                             Environment.NewLine));

            codeBuilder.Append(string.Format("{0} do {1}", Utilities.TAB,
                                  Environment.NewLine));

            codeBuilder.Append(string.Format("{0} :: index <  actioncount -> {1}", Utilities.TAB, Environment.NewLine));

            codeBuilder.Append(
                string.Format("{0}{0} included_actions_set[index] = (actions_array[index].included == 1 -> 1: 0); {1}",
                              Utilities.TAB, Environment.NewLine));

            codeBuilder.Append(
                string.Format("{0}{0} index = index + 1; {1}",
                              Utilities.TAB, Environment.NewLine));


            codeBuilder.Append(
                string.Format("{0} :: else -> 	break; {1}",
                              Utilities.TAB, Environment.NewLine));


            codeBuilder.Append(string.Format("{0} od; {1}",Utilities.TAB, Environment.NewLine));

        }

        #endregion



    }
}
