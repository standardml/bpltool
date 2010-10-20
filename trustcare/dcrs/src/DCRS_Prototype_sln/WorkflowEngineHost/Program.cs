using System;
using System.Collections.Generic;
using System.Linq;
using System.ServiceModel;
using System.Text;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;
using ITU.DK.DCRS.WorkflowEngine.Core;

namespace SampleConsoleTestApplication
{
    class Program
    {
        static void Main(string[] args)
        {
            try
            {
                var workflowEngineInstance = DCRSWorkflowEngine.WorkflowEngineInstance;


                Console.WriteLine("Hosted the DCRS workflow engine successfully!");

                while (true)
                {
                    var line = Console.ReadLine();

                    if (line != null) if(line.Equals("exit")) break;
                }

            }
            catch (Exception exception)
            {

                Console.WriteLine("Failed to host workflow engine. Error message:{0}", exception.Message);
            }
        }
    }
}
