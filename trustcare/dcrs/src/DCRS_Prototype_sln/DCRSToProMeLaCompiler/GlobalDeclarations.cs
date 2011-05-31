using System;
using System.Text;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSToProMeLaCompiler
{
    internal static class GlobalDeclarations
    {


        internal static void WriteDeclarationsFiniteRuns(DCRSModel model, ref StringBuilder codeBuilder)
        {

            // Write Comment
            WriteComment(model, codeBuilder);

            // Write Header definitions.
            WriteHeaders(model, codeBuilder);
            
            // Write Array definitions.
            WriteArrayDeclarations(model, codeBuilder);

            // Write Variable definitions.
            WriteVariableDeclarations(codeBuilder);


        }


        internal static void WriteDeclarationsForInfiniteRuns(DCRSModel model, ref StringBuilder codeBuilder)
        {

            // Write Comment
            WriteComment(model, codeBuilder);

            // Write Header definitions.
            WriteHeaders(model, codeBuilder);

            // Write Array definitions.
            WriteArrayDeclarations(model, codeBuilder);

            // Write Variable definitions.
            WriteVariableDeclarations(codeBuilder);

            // Write additional variables for acceptance over infinite runs.
            WriteAdditionalVariableDeclarations(codeBuilder);


        }


        private static void WriteComment(DCRSModel model, StringBuilder codeBuilder)
        {

            codeBuilder.Append("/*");

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append(string.Format(
                                   "DCRS Example: {0}  PROMELA  language code for model checking in SPIN tool.",
                                   model.ModelName));

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append(string.Format(
                                   "Generated on {0} by DCRStoPROMELA Compiler.",
                                   DateTime.Now.ToString("yyyy-MM-ddTHH:mm:ss")));

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append("Developed by Raghava Rao Mukkamala (rao@itu.dk)");

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append("*/");

            codeBuilder.Append(Environment.NewLine);


        }
        
        private static void WriteHeaders(DCRSModel model, StringBuilder codeBuilder)
        {

            //#define actioncount	3
            //#define OM	0
            //#define S	1
            //#define GM	2


            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append(string.Format(
                                   "#define actioncount {0}{1}",
                                   model.ActionList.Count, Environment.NewLine));


            foreach (var key in model.ActionList.Keys)
            {

                codeBuilder.Append(string.Format(
                                       "#define {0} {1}{2}",
                                       model.ActionList[key], key, Environment.NewLine));
            }


            codeBuilder.Append(Environment.NewLine);



        }

        private static void WriteArrayDeclarations(DCRSModel model, StringBuilder codeBuilder)
        {


            //typedef action {byte actionid; bit included };

            //action actions_array[actioncount];

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append("typedef action {byte actionid; bit included };");

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append(string.Format(
                                   "action actions_array[{0}]; {1}",
                                   model.ActionList.Count, Environment.NewLine));


            //typedef ndimarray {bit column[actioncount]};

            //ndimarray condition_relation[actioncount];

            //ndimarray response_relation[actioncount];

            //ndimarray include_relation[actioncount];

            //ndimarray exclude_relation[actioncount];

            //ndimarray milestone_relation[actioncount];



            codeBuilder.Append("typedef ndimarray {bit column[");

            codeBuilder.Append(model.ActionList.Count);

            codeBuilder.Append("]}; ");

            codeBuilder.Append(Environment.NewLine);

            //codeBuilder.Append(Utilities.TAB);

            //codeBuilder.Append(string.Format(
            //           "typedef ndimarray {bit column[{0}]}; {1}",
            //           model.ActionList.Count, Environment.NewLine));
// ReSharper restore FormatStringProblem

            codeBuilder.Append(string.Format(
                                   "ndimarray condition_relation[{0}]; {1}",
                                   model.ActionList.Count, Environment.NewLine));


            codeBuilder.Append(string.Format(
                                   "ndimarray response_relation[{0}]; {1}",
                                   model.ActionList.Count, Environment.NewLine));


            codeBuilder.Append(string.Format(
                                   "ndimarray include_relation[{0}]; {1}",
                                   model.ActionList.Count, Environment.NewLine));


            codeBuilder.Append(string.Format(
                                   "ndimarray exclude_relation[{0}]; {1}",
                                   model.ActionList.Count, Environment.NewLine));


            codeBuilder.Append(string.Format(
                       "ndimarray milestone_relation[{0}]; {1}",
                       model.ActionList.Count, Environment.NewLine));


            ///* Declarations of sets included_actions_set, executed_actions_set, pending_responses_set */
            //bit included_actions_set[actioncount];
            //bit executed_actions_set[actioncount];
            //bit pending_responses_set[actioncount];
            //bit enabled_actions_set[actioncount];



            codeBuilder.Append("/* Declarations of sets included_actions_set, executed_actions_set, pending_responses_set */");

            codeBuilder.Append(Environment.NewLine);


            codeBuilder.Append(string.Format(
                                   "bit included_actions_set[{0}]; {1}",
                                   model.ActionList.Count, Environment.NewLine));


            codeBuilder.Append(string.Format(
                                   "bit executed_actions_set[{0}]; {1}",
                                   model.ActionList.Count, Environment.NewLine));

            codeBuilder.Append(string.Format(
                                   "bit pending_responses_set[{0}]; {1}",
                                   model.ActionList.Count, Environment.NewLine));


            codeBuilder.Append(string.Format(
                                   "bit enabled_actions_set[{0}]; {1}",
                                   model.ActionList.Count, Environment.NewLine));


            
            codeBuilder.Append(Environment.NewLine);



        }


        private static void WriteVariableDeclarations(StringBuilder codeBuilder)
        {


            codeBuilder.Append(Environment.NewLine);

            ////bit deadlock_detected = 0;
            //codeBuilder.Append(string.Format(
            //                       "bit deadlock_detected = 0; {0}",
            //                       Environment.NewLine));


            //byte index = 0;
            codeBuilder.Append(string.Format(
                                   "byte index = 0; {0}",
                                   Environment.NewLine));

            //byte index2 = 0;
            codeBuilder.Append(string.Format(
                                   "byte index2 = 0; {0}",
                                   Environment.NewLine));

            //short actions_executed_count = 0;
            codeBuilder.Append(string.Format(
                                   "short actions_executed_count = 0; {0}",
                                   Environment.NewLine));


            //bit state_accepted = 1;
            codeBuilder.Append(string.Format(
                                   "bit state_accepted = 1; {0}",
                                   Environment.NewLine));

            //bit accepted_state_reached = 0;
            codeBuilder.Append(string.Format(
                                   "bit accepted_state_reached = 0; {0}",
                                   Environment.NewLine));
            //bit can_execute = 1;
            codeBuilder.Append(string.Format(
                                   "bit can_execute = 1; {0}",
                                   Environment.NewLine));
            //byte loopindex = 0;
            codeBuilder.Append(string.Format(
                                   "byte loopindex = 0; {0}",
                                   Environment.NewLine));

            //byte random_action_executed = 0;
            //show byte random_action_executed = actioncount + 1;
            codeBuilder.Append(string.Format(
                                   "show byte random_action_executed = actioncount + 1; {0}",
                                   Environment.NewLine));

            codeBuilder.Append(Environment.NewLine);


        }

        private static void WriteAdditionalVariableDeclarations(StringBuilder codeBuilder)
        {
            
            
            codeBuilder.Append(Environment.NewLine);


            ///* New Variables for acceptance over infinite runs. */
            codeBuilder.Append(string.Format(
                                  "/* New Variables for acceptance over infinite runs. */ {0}",
                                  Environment.NewLine));
            
            //byte state_index = 0;
             codeBuilder.Append(string.Format(
                                   "byte state_index = 0; {0}",
                                   Environment.NewLine));

            //bit include_response_current[actioncount];
             codeBuilder.Append(string.Format(
                       "bit include_response_current[actioncount]; {0}",
                       Environment.NewLine));
            
            //bit included_actions_nextstate[actioncount];
             codeBuilder.Append(string.Format(
                       "bit included_actions_nextstate[actioncount]; {0}",
                       Environment.NewLine));


            //bit pending_responses_nextstate[actioncount];
             codeBuilder.Append(string.Format(
                       "bit pending_responses_nextstate[actioncount]; {0}",
                       Environment.NewLine));
            
            //bit include_response_nextstate[actioncount];
             codeBuilder.Append(string.Format(
                       "bit include_response_nextstate[actioncount]; {0}",
                       Environment.NewLine));


            //bit acceptable_responses_set[actioncount];
             codeBuilder.Append(string.Format(
                       "bit acceptable_responses_set[actioncount]; {0}",
                       Environment.NewLine));


            //bit m_set[actioncount];
             codeBuilder.Append(string.Format(
                       "bit m_set[actioncount]; {0}",
                       Environment.NewLine));


            //bit min_include_response_current;
             codeBuilder.Append(string.Format(
                       "byte min_include_response_current; {0}",
                       Environment.NewLine));


            //bit min_m_set;
             codeBuilder.Append(string.Format(
                       "byte min_m_set; {0}",
                       Environment.NewLine));


            //byte m_set_count = 0;
             codeBuilder.Append(string.Format(
                       "byte m_set_count = 0; {0}",
                       Environment.NewLine));


            //byte include_response_current_set_count = 0;
             codeBuilder.Append(string.Format(
                       "byte include_response_current_set_count = 0; {0}",
                       Environment.NewLine));


            //byte include_response_nextstate_set_count = 0;
             codeBuilder.Append(string.Format(
                       "byte include_response_nextstate_set_count = 0; {0}",
                       Environment.NewLine));


            //bit accepting_state_visited = 0;
             codeBuilder.Append(string.Format(
                       "bit accepting_state_visited = 0; {0}",
                       Environment.NewLine));

            codeBuilder.Append(Environment.NewLine);
            
        }

        
    }
}
