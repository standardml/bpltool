using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSToZingCompiler
{
    public static class GlobalDeclarations
    {

        internal static void WriteDeclarationsCommon(DCRSModel model, ref StringBuilder codeBuilder)
        {

            // Write Comment
            WriteComment(model, codeBuilder);


            // Write Type declarations..
            WriteTypeDeclarations(model, codeBuilder);

            // Write declarations needed for model checking.
            WriteModelCheckingSpecificDeclarations(model, codeBuilder);

        }




        private static void WriteComment(DCRSModel model, StringBuilder codeBuilder)
        {

            codeBuilder.Append("/*");

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append(string.Format(
                                   "DCRS Example: {0}  Code for model checking in Zing tool.",
                                   model.ModelName));

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append(string.Format(
                                   "Generated on {0} by DCRStoZing Compiler.",
                                   DateTime.Now.ToString("yyyy-MM-ddTHH:mm:ss")));

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append("Developed by Raghava Rao Mukkamala (rao@itu.dk)");

            codeBuilder.Append(Environment.NewLine);

            codeBuilder.Append("*/");

            codeBuilder.Append(Environment.NewLine);


        }

        private static void WriteTypeDeclarations(DCRSModel model, StringBuilder codeBuilder)
        {

            codeBuilder.Append(Environment.NewLine);

            //// Section for Common Declarations
            codeBuilder.Append(string.Format(
                "// Section for Common Type Declarations {0}",
                 Environment.NewLine));

            //enum TypeActionsEnum {pm, s, gm, dt };
            codeBuilder.Append("enum TypeActionsEnum {");
           
            var actionsList = model.ActionList.Keys.Aggregate(string.Empty, (current, key) => current + string.Format("{0},", key));

            if (actionsList.EndsWith(",")) actionsList = actionsList.Remove(actionsList.Length - 1, 1);

            codeBuilder.Append(string.Format(
                "{0} }; {1}",
                actionsList, Environment.NewLine));

            
            // Set Declarations...

            //set TypeActionsSet TypeActionsEnum;
            codeBuilder.Append(string.Format(
                "set TypeActionsSet TypeActionsEnum; {0}",
                Environment.NewLine));

            //set TypeRelationsSet Relation;
            codeBuilder.Append(string.Format(
                "set TypeRelationsSet Relation; {0}",
                Environment.NewLine));




        }

        private static void WriteModelCheckingSpecificDeclarations(DCRSModel model, StringBuilder codeBuilder)
        {
            // Right now we dont have any, if we have some specific declaration, then they go here.


        }


    }
}
