using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.Visualization.Elements;

namespace DCRSGraphicalEditor
{
    public enum PrimitiveType { Condition=0, Response, Include, Exclude };

    public class DCRSProcessHandler
    {
        DCRSProcess Process;
        public DCRSProcessHandler(DCRSProcess p)
        {
            Process = p;
        }


        public short AddAction()
        {
            short maxNode;
            if (Process.Specification.ActionList.Count == 0)
                maxNode = -1;
            else
                maxNode = Process.Specification.ActionList.Max(x => x.Key);
            short newNode = (short)(maxNode + 1);
            Process.Specification.ActionList.Add(newNode, "Action " + newNode.ToString());
            Process.Specification.ActionsToRolesDictionary.Add(newNode, new List<string>());
            return newNode;
        }


        public void RemoveAction(short a)
        {
            if (Process.Runtime.CurrentState.EnabledActions.Contains(a))
                Process.Runtime.CurrentState.EnabledActions.Remove(a);

            if (Process.Runtime.CurrentState.StateVector.IncludedActions.Contains(a))
                Process.Runtime.CurrentState.StateVector.IncludedActions.Remove(a);

            InnerRemoveAllActionPrimitives(ref Process.Specification.Conditions, a);
            InnerRemoveAllActionPrimitives(ref Process.Specification.Responses, a);
            InnerRemoveAllActionPrimitives(ref Process.Specification.Excludes, a);
            InnerRemoveAllActionPrimitives(ref Process.Specification.Includes, a);

            Process.Specification.ActionsToRolesDictionary.Remove(a);
            Process.Specification.ActionList.Remove(a);
        }


        private void InnerRemoveAllActionPrimitives(ref short[,] a, short act)
        {
            for (var index = 0; index < a.GetLength(0); index++)
            {
                if ((a[index, 0] == act) || (a[index, 1] == act))
                {
                    InnerRemovePrimitive(ref a, a[index, 0], a[index, 1]);
                    index--;
                }
            }
        }



        public void AddCondition(short s, short d)
        {
            InnerAddPrimitive(ref Process.Specification.Conditions, s, d);
        }

        public void AddResponse(short s, short d)
        {
            InnerAddPrimitive(ref Process.Specification.Responses, s, d);
        }

        public void AddExclude(short s, short d)
        {
            InnerAddPrimitive(ref Process.Specification.Excludes, s, d);
        }

        public void AddInclude(short s, short d)
        {
            InnerAddPrimitive(ref Process.Specification.Includes, s, d);
        }


        private void InnerAddPrimitive(ref short[,] a, short s, short d)
        {
            short[,] temp;

            temp = new short[a.GetLength(0) + 1, 2];

            for (var index = 0; index < a.GetLength(0); index++)
            {
                temp[index, 0] = a[index, 0];
                temp[index, 1] = a[index, 1];
            }

            temp[a.GetLength(0), 0] = s;
            temp[a.GetLength(0), 1] = d;

            a = temp;
        }

        public void RemoveCondition(short s, short d)
        {
            InnerRemovePrimitive(ref Process.Specification.Conditions, s, d);
        }

        public void RemoveResponse(short s, short d)
        {
            InnerRemovePrimitive(ref Process.Specification.Responses, s, d);
        }

        public void RemoveExclude(short s, short d)
        {
            InnerRemovePrimitive(ref Process.Specification.Excludes, s, d);
        }

        public void RemoveInclude(short s, short d)
        {
            InnerRemovePrimitive(ref Process.Specification.Includes, s, d);
        }

        private void InnerRemovePrimitive(ref short[,] a, short s, short d)
        {
            short[,] temp;

            temp = new short[a.GetLength(0) - 1, 2];

            int indext = 0;

            for (var index = 0; index < a.GetLength(0); index++)
            {
                if ((a[index, 0] != s) || (a[index, 1] != d))
                {
                    temp[indext, 0] = a[index, 0];
                    temp[indext, 1] = a[index, 1];
                    indext++;
                }
            }
            a = temp;
        }


        public void RemovePrimitive(Arrow a)
        {
            if (a.GetType() == typeof(ConditionArrow))
            {
                RemoveCondition(a.DestinationNode.Id, a.SourceNode.Id);
            }
            else if (a.GetType() == typeof(ResponseArrow))
            {
                RemoveResponse(a.SourceNode.Id, a.DestinationNode.Id);
            }
            else if (a.GetType() == typeof(ExclusionArrow))
            {
                RemoveExclude(a.SourceNode.Id, a.DestinationNode.Id);
            }
            else if (a.GetType() == typeof(InclusionArrow))
            {
                RemoveInclude(a.SourceNode.Id, a.DestinationNode.Id);
            }
            else if (a.GetType() == typeof(ConditionReponseArrow))
            {
                RemoveCondition(a.DestinationNode.Id, a.SourceNode.Id);
                RemoveResponse(a.SourceNode.Id, a.DestinationNode.Id);
            }
            else if (a.GetType() == typeof(MutualExclusionArrow))
            {
                RemoveExclude(a.SourceNode.Id, a.DestinationNode.Id);
                RemoveExclude(a.DestinationNode.Id, a.SourceNode.Id);
            }
        }
    }
}
