using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DCRSToProMeLaCompiler;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRSUIApplication
{
    public class SampleDCRSExamples
    {

        public static DCRSModel GetGiveMedicineSmall()
        {

            var actionsList = new Dictionary<short, string> {{0, "OM"}, {1, "S"}, {2, "GM"}};

            var conditions = new short[2,2] {{1, 0}, {2, 1}};

            var responses = new short[2, 2];

            responses[0, 0] = 0;

            responses[0, 1] = 1;

            responses[1, 0] = 0;

            responses[1, 1] = 2;

            var includes = new short[0, 0];

            var excludes = new short[0, 0];

            //var initialState = new Dictionary<short, bool> {{0, true}, {1, true}, {2, true}};

            var initialState = new short[3,2] {{0, 1}, {1, 1}, {2, 1}};
            


            var model = new DCRSModel("GiveMedicine_short",actionsList, includes, excludes, responses, conditions )
                            {InitialState = initialState};




            return model;
            

        }



    }
}
