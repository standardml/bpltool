using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSToZingCompiler
{
    internal static class RelationClassDeclaration
    {


        //class Relation 
        //{
        //    TypeActionsEnum Child;
        //    TypeActionsEnum Parent;
        //    void Initialise(TypeActionsEnum dom, TypeActionsEnum ran)
        //    {
        //        Parent = dom;
        //        Child = ran;
        //    }
        //};

        internal static void WriteRelationClass(DCRSModel model, ref StringBuilder codeBuilder)
        {

            codeBuilder.Append(Environment.NewLine);


            //class Relation 
            codeBuilder.Append(string.Format(
                "class Relation {0}",
                Environment.NewLine));


            //{ 
            codeBuilder.Append("{" + Environment.NewLine);

            //    TypeActionsEnum Child;
            codeBuilder.Append(string.Format("{0} TypeActionsEnum Child; {1}", Utilities.TAB,
                                             Environment.NewLine));




            //    TypeActionsEnum Parent;
            codeBuilder.Append(string.Format("{0} TypeActionsEnum Parent; {1}", Utilities.TAB,
                                             Environment.NewLine));



            //    void Initialise(TypeActionsEnum dom, TypeActionsEnum ran)
            codeBuilder.Append(string.Format("{0} void Initialise(TypeActionsEnum dom, TypeActionsEnum ran) {1}",
                                             Utilities.TAB,
                                             Environment.NewLine));

            //    {
            codeBuilder.Append(Utilities.TAB + "{" + Environment.NewLine);


            //        Parent = dom;
            codeBuilder.Append(string.Format("{0}{0} Parent = dom; {1}", Utilities.TAB,
                                                         Environment.NewLine));
            //        Child = ran;
            codeBuilder.Append(string.Format("{0}{0} Child = ran; {1}", Utilities.TAB,
                                                         Environment.NewLine));
            //    }
            codeBuilder.Append(string.Format("{0} } {1}", Utilities.TAB,
                                             Environment.NewLine));

            //};
            codeBuilder.Append(string.Format("}; {0}", 
                                             Environment.NewLine));






        }






    }
}
