using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRStoFiniteAutomaton
{
    public class DCRSExamples
    {



        public static DCRSModel GetGiveMedicineFullExample()
        {

            var actionsList = new Dictionary<short, string> { { 0, "pm" }, { 1, "s" }, { 2, "gm" }, { 3, "dt" } };

            var conditions = new short[3, 2] { { 1, 0 }, { 2, 1 }, { 3, 1 } };

            var responses = new short[3, 2];

            responses[0, 0] = 0;

            responses[0, 1] = 1;

            responses[1, 0] = 0;

            responses[1, 1] = 2;

            responses[2, 0] = 3;

            responses[2, 1] = 1;


            var includes = new short[2, 2] { { 1, 2 }, { 1, 3 } };

            var excludes = new short[2, 2] { { 2, 3 }, { 3, 2 } };

            //var initialState = new Dictionary<short, bool> {{0, true}, {1, true}, {2, true}};

            var initialState = new short[4, 2] { { 0, 1 }, { 1, 1 }, { 2, 1 }, { 3, 1 } };

            var model = new DCRSModel("GiveMedicine_full",actionsList, includes, excludes, responses, conditions ) { InitialState = initialState };

            //string result = DCRSCompiler.ComplileDcrsModelForStrongAcceptanceCondition(model, @"D:\PhDWork\Temp");



            return model;


        }

        public static DCRSModel GetGiveMedicineSmallExample()
        {


            var actionsList = new Dictionary<short, string> { { 0, "pm" }, { 1, "s" }, { 2, "gm" } };

            var conditions = new short[2, 2] { { 1, 0 }, { 2, 1 } };

            var responses = new short[2, 2];

            responses[0, 0] = 0;

            responses[0, 1] = 1;

            responses[1, 0] = 0;

            responses[1, 1] = 2;

            var includes = new short[0, 0];

            var excludes = new short[0, 0];

            //var initialState = new Dictionary<short, bool> {{0, true}, {1, true}, {2, true}};

            var initialState = new short[3, 2] { { 0, 1 }, { 1, 1 }, { 2, 1 } };

            var model = new DCRSModel("GiveMedicine_small", actionsList, includes, excludes, responses, conditions) { InitialState = initialState };

            //string result = DCRSCompiler.ComplileDcrsModelForStrongAcceptanceCondition(model, @"D:\PhDWork\Temp");



            return model;
        }


        public static DCRSModel GetTwoSelfResponseEventsExample()
        {

            var actionsList = new Dictionary<short, string> { { 0, "a" }, { 1, "b" } };

            var conditions = new short[0,0];

            var responses = new short[2, 2];

            responses[0, 0] = 0;

            responses[0, 1] = 0;

            responses[1, 0] = 1;

            responses[1, 1] = 1;

            var includes = new short[0,0];

            var excludes = new short[0,0];

            //var initialState = new Dictionary<short, bool> {{0, true}, {1, true}, {2, true}};

            var initialState = new short[2,2] {{0, 1}, {1, 1}};

            var model = new DCRSModel("TwinSelfResponseActions",actionsList, includes, excludes, responses, conditions ) { InitialState = initialState };

            //string result = DCRSCompiler.ComplileDcrsModelForStrongAcceptanceCondition(model, @"D:\PhDWork\Temp");

            return model;


        }


        public static DCRSModel GetGiveMedicineSmallExampleWithTauAction()
        {


            var actionsList = new Dictionary<short, string> { { 0, "pm" }, { 1, "s" }, { 2, "gm" }, { 3, "tau" } };

            var conditions = new short[2, 2] { { 1, 0 }, { 2, 1 } };

            var responses = new short[2, 2];

            responses[0, 0] = 0;

            responses[0, 1] = 1;

            responses[1, 0] = 0;

            responses[1, 1] = 2;

            var includes = new short[0, 0];

            var excludes = new short[0, 0];

            //var initialState = new Dictionary<short, bool> {{0, true}, {1, true}, {2, true}};

            var initialState = new short[4, 2] { { 0, 1 }, { 1, 1 }, { 2, 1 }, { 3, 1 } };

            var model = new DCRSModel("GiveMedicine_small_tau", actionsList, includes, excludes, responses, conditions) { InitialState = initialState };

            //string result = DCRSCompiler.ComplileDcrsModelForStrongAcceptanceCondition(model, @"D:\PhDWork\Temp");



            return model;
        }



        public static DCRSModel GetArrangeMeetingSample()
        {


            var actionsList = new Dictionary<short, string> { { 0, "Cc" }, { 1, "PdLO" }, { 2, "PdDA" }, { 3, "ALO" }, { 4, "ADA" }, { 5, "Hm" } };


            // {pldo, cc}, (plda, pldo)
            var conditions = new short[2, 2] { { 1, 0 }, { 2, 1 } };

            var strongConditions = new short[0, 0];

            var responses = new short[7,2] {{0, 1}, {1, 4}, {2, 3}, {1, 5}, {2, 5}, {3, 5}, {4, 5}};

            // {pldo, cc}, (plda, pldo)
            var milestones = new short[4,2] {{5, 1}, {5, 2}, {5, 3}, {5, 4}};

            var includes = new short[2, 2] { { 1, 4 }, { 2, 3 } };

            var excludes = new short[4,2] {{3, 4}, {4, 3}, {3, 3}, {4, 4}};

            var initialState = new short[6, 2] { { 0, 1 }, { 1, 1 }, { 2, 1 }, { 3, 0 }, { 4, 0 }, { 5, 1 } };

            var dcrsmodel = new DCRSModel("Arrange-Meeting-Example", actionsList, includes, excludes, responses,
                                          conditions, strongConditions, milestones) {InitialState = initialState};



            return dcrsmodel;


        }




    }
}
