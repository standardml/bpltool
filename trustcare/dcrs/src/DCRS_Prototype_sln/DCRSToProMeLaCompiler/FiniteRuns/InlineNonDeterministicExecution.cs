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


            codeBuilder.Append(string.Format("{0} :: else -> {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append(string.Format("{0} dead_lock_reached:	assert(false); {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append(string.Format("{0} fi; {1}", Utilities.TAB,
                                             Environment.NewLine));


            codeBuilder.Append("}");

            codeBuilder.Append(Environment.NewLine);









        }

    }
}