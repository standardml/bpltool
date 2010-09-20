using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSToZingCompiler
{
    public class DCRSToZingCompiler
    {

        private static StringBuilder codeBuilder = new StringBuilder();

        private static DCRSModel dcrsmodel;

        public static string ComplileDcrsModelForStrongAcceptanceCondition(DCRSModel model, string folderPath)
        {

            // Flush the contents if any.
            codeBuilder.Length = 0;

            // Write Global Declarations.
            GlobalDeclarations.WriteDeclarationsCommon(model, ref codeBuilder);

            
            // Write the Relation Class.
            RelationClassDeclaration.WriteRelationClass(model, ref codeBuilder);








            
            
            return string.Empty;
        }
    }
}
