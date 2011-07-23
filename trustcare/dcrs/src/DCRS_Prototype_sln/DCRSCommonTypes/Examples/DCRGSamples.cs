using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace ITU.DK.DCRS.CommonTypes.Examples
{
    public static class DCRGSamples
    {



        public static DCRSModel GetCaseHandlingDCRGProcess()
        {


            var actionsList = new Dictionary<short, string>
                                  {
                                      { 0, "Sub" }, { 1, "ACI" }, { 2, "MD" }, { 3, "DA" }, { 4, "EM" },{ 5, "UL" }, { 6, "DL" }, { 7, "PLO" }, { 8, "ALO" }, { 9, "ADA" }, { 10, "PDA" }, { 11, "HM" }
                                  };


            // 
            var conditions = new short[9, 2] { { 1, 0 }, { 0, 2 }, { 0, 3 }, { 7, 1 }, { 4, 1 }, { 5, 1 }, { 6, 1 }, { 6, 5 }, { 10, 7 } };

            var strongConditions = new short[0, 0];

            var responses = new short[8, 2] { { 0, 1 }, { 0, 7 }, { 7, 9 }, { 10, 8 }, { 7, 11 }, { 8, 11 }, { 9, 11 }, { 10, 11 } };

            // {pldo, cc}, (plda, pldo)
            var milestones = new short[4, 2] { { 11, 7 }, { 11, 8 }, { 11, 9 }, { 11, 10 } };

            var includes = new short[2, 2] { { 7, 9 }, { 10, 8 } };

            var excludes = new short[5, 2] { { 0, 0 }, { 8, 8}, { 8, 9 }, { 9, 9 }, { 9, 8 }};

            var initialState = new short[12, 2] { { 0, 1 }, { 1, 1 }, { 2, 1 }, { 3, 1 }, { 4, 1 }, { 5, 1 }, { 6, 1 }, { 7, 1 }, { 8, 0 }, { 9, 0 }, { 10, 1 }, { 11, 1 } };

            var dcrsmodel = new DCRSModel("CaseHandlingProcess", actionsList, includes, excludes, responses,
                                          conditions, strongConditions, milestones) { InitialIncludedActions = initialState };



            return dcrsmodel;


        }

        public static DCRSModel GetCursePrayExample()
        {


            var actionsList = new Dictionary<short, string>
                                  {
                                      { 0, "bless" }, { 1, "pray" }, { 2, "curse" }
                                  };


            // 
            var conditions = new short[0,0];

            var strongConditions = new short[0, 0];

            var responses = new short[1, 2] { { 2, 1 }};

            // {pldo, cc}, (plda, pldo)
            var milestones = new short[0,0]; 

            var includes = new short[0, 0];

            var excludes = new short[0,0];

            var initialState = new short[3, 2] { { 0, 1 }, { 1, 1 }, { 2, 1 }};

            var dcrsmodel = new DCRSModel("cursepray", actionsList, includes, excludes, responses,
                                          conditions, strongConditions, milestones) { InitialIncludedActions = initialState };



            return dcrsmodel;


        }

        public static DCRSModel GetCursePrayExampleStrict()
        {


            var actionsList = new Dictionary<short, string>
                                  {
                                      { 0, "bless" }, { 1, "pray" }, { 2, "curse" }
                                  };


            // 
            var conditions = new short[0, 0];

            var strongConditions = new short[0, 0];

            var responses = new short[1, 2] { { 2, 1 } };

            // {pldo, cc}, (plda, pldo)
            var milestones = new short[0, 0];

            var includes = new short[1, 2] { { 2, 1 } };

            var excludes = new short[8, 2] { { 0, 0 }, { 1, 1 }, { 2, 2 }, { 0, 1 }, { 0, 2 }, { 1, 0 }, { 1, 2 }, { 2, 0 } };

            var initialState = new short[3, 2] { { 0, 1 }, { 1, 1 }, { 2, 1 } };

            var dcrsmodel = new DCRSModel("cursepraystrict", actionsList, includes, excludes, responses,
                                          conditions, strongConditions, milestones) { InitialIncludedActions = initialState };



            return dcrsmodel;


        }








    }
}
