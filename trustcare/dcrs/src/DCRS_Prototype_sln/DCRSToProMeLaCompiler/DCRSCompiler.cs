using System.Text;
using System.IO;
using DCRSToProMeLaCompiler.FiniteRuns;
using DCRSToProMeLaCompiler.InfiniteRuns;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSToProMeLaCompiler
{
    public static class DCRSCompiler
    {

        private static StringBuilder codeBuilder = new StringBuilder();

        private static DCRSModel dcrsmodel;

        public static string ComplileDcrsModelForStrongAcceptanceCondition(DCRSModel model, string folderPath)
        {
            dcrsmodel = model;

            // Flush the contents if any.
            codeBuilder.Length = 0;

            GlobalDeclarations.WriteDeclarationsFiniteRuns(model, ref codeBuilder);


            // Write the inline model_specification
            InlineSpecification.WriteModelSpecification(model, ref codeBuilder);

            InlineEnabledActions.ClearEnabledActionsList(model, ref codeBuilder);

            InlineEnabledActions.ComputeEnabledActionsList(model, ref codeBuilder);

            InlineNonDeterministicExecution.WriteNonDeterministicExecution(model, ref codeBuilder);

            InlineStateComputation.WriteStateComputationAfterExecution(model, ref codeBuilder);

            
            InlineAcceptingConditionCheck.WriteAcceptingConditionOverFiniteTrace(model, ref codeBuilder);

            DcrsPromelaProctype.WriteDCRSProcessDeclarationForFiniteComputation(model, ref codeBuilder);


            var filepath = GetFileSavePath(folderPath);

            File.WriteAllText(filepath, codeBuilder.ToString());


            return filepath;



        }



        public static string ComplileDcrsModelWeakerAcceptanceCondition(DCRSModel model, string folderPath)
        {
            dcrsmodel = model;

            // Flush the contents if any.
            codeBuilder.Length = 0;

            GlobalDeclarations.WriteDeclarationsForInfiniteRuns(model, ref codeBuilder);


            // Write the inline model_specification
            InlineSpecification.WriteModelSpecification(model, ref codeBuilder);

            InlineEnabledActions.ComputeEnabledActionsList(model, ref codeBuilder);


            InlineNonDeterministicExecution.WriteNonDeterministicExecution(model, ref codeBuilder);

            InlineComputeResponseSets.WriteInlineComputeIncludeResponseSets(model, ref codeBuilder);

            InlineComputeResponseSets.WriteInlineMinimunIncludeResponseSets(model, ref codeBuilder);

            CheckAcceptanceCondition.WriteInlinecheckStateAcceptanceCondition(model, ref codeBuilder);
            
            
            InlineStateComputation.WriteStateComputationAfterExecution(model, ref codeBuilder);


            DcrsPromelaProctype.WriteDCRSProcessDeclarationForInFiniteRuns(model, ref codeBuilder);


            var filepath = GetFileSavePath(folderPath);

            File.WriteAllText(filepath, codeBuilder.ToString());


            return filepath;

        }


        private static string GetFileSavePath(string folderPath)
        {
            // To add DateTime.Now.ToString("yyyyMMddHHmm") to get Unique file name...
            return string.Format("{0}\\{1}_Promelacode.c", folderPath, dcrsmodel.ModelName
                );




        }









    }
}
