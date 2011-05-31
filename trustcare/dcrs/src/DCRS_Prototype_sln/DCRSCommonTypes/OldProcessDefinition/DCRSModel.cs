using System.Collections.Generic;

namespace ITU.DK.DCRS.CommonTypes.OldProcessDefinition
{
    public class DCRSModel
    {
        private readonly Dictionary<short, string> actionList;


        /// <summary>
        /// The first and second columns refere to Ids of actions in that relation.
        /// </summary>
        private readonly short[,] includes;
        private readonly short[,] excludes;
        private readonly short[,] responses;
        private readonly short[,] conditions;
        private readonly short[,] strongconditions;
        private readonly short[,] milestones;
        private readonly string modelName;

        public List<short> NonConditionalEvents = new List<short>();
        
        /// <summary>
        /// The first column is an Id of an action and the 2nd column referes to included/excluded : 1\0
        /// </summary>
        public short[,] InitialState { get; set; }

        public short[] InitialPendingResponses { get; set; }



        public DCRSModel(string modelName, Dictionary<short,string> actionList, short[,] includes, short[,] excludes, short[,] responses, short[,] conditions) 
        {
          
            //DCRSModel(modelName, actionList, includes, excludes, responses, conditions, strongConditionsArray);
            
            this.actionList = actionList;
            this.includes = includes;
            this.excludes = excludes;
            this.responses = responses;
            this.conditions = conditions;
            this.modelName = modelName;
            
            // Assign default null array for strong conditions.
            strongconditions = new short[0, 2];
            milestones = new short[0, 2];


            calculateNonConditionalEvents();
        }


        public DCRSModel(string modelName, Dictionary<short, string> actionList, short[,] includes, short[,] excludes, short[,] responses, short[,] conditions, short[,] strongconditions )
        {
            this.actionList = actionList;
            this.includes = includes;
            this.excludes = excludes;
            this.responses = responses;
            this.conditions = conditions;
            this.strongconditions = strongconditions;
            this.modelName = modelName;

            milestones = new short[0, 2];


            calculateNonConditionalEvents();
        }



        public DCRSModel(string modelName, Dictionary<short, string> actionList, short[,] includes, short[,] excludes, short[,] responses, short[,] conditions, short[,] strongconditions, short [,] milestones)
        {
            this.actionList = actionList;

            this.includes = includes;
            this.excludes = excludes;
            this.responses = responses;
            this.conditions = conditions;
            this.strongconditions = strongconditions;

            this.milestones = milestones;
            
            this.modelName = modelName;

            calculateNonConditionalEvents();
        }



        public string ModelName
        {
            get { return modelName; }
        }

        public short[,] Conditions
        {
            get { return conditions; }
        }


        public short[,] StrongConditions
        {
            get { return strongconditions; }
        }


        public short[,] MileStones
        {
            get { return milestones; }
        }


        public short[,] Responses
        {
            get { return responses; }
        }

        public short[,] Excludes
        {
            get { return excludes; }
        }

        public short[,] Includes
        {
            get { return includes; }
        }

        public Dictionary<short, string> ActionList
        {
            get { return actionList; }
        }


        private void calculateNonConditionalEvents()
        {
            // Add all events.
            NonConditionalEvents.AddRange(actionList.Keys);

            for (var index = 0; index < conditions.GetLength(0); index++)
            {
                // Remove all the events which are conditions to other events.
                NonConditionalEvents.Remove(conditions[index, 1]);

            }



        }

    }
}
