using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes;

namespace ITU.DK.DCRS.Visualization
{
    class ShortHandFinder
    {
        public Dictionary<short, Dictionary<short, bool>> Responses;
        public Dictionary<short, Dictionary<short, bool>> Conditions;
        public Dictionary<short, Dictionary<short, bool>> Excludes;
        public Dictionary<short, Dictionary<short, bool>> Includes;
        public Dictionary<short, Dictionary<short, bool>> Mutexes;
        public Dictionary<short, Dictionary<short, bool>> ConditionResponses;

        public ShortHandFinder(DCRS.CommonTypes.Process.DCRSSpecification spec)
        {
            Responses = new Dictionary<short, Dictionary<short, bool>>();
            Conditions = new Dictionary<short, Dictionary<short, bool>>();
            Excludes = new Dictionary<short, Dictionary<short, bool>>();
            Includes = new Dictionary<short, Dictionary<short, bool>>();
            Mutexes = new Dictionary<short, Dictionary<short, bool>>();
            ConditionResponses = new Dictionary<short, Dictionary<short, bool>>();

            for (var index = 0; index < spec.Conditions.GetLength(0); index++)
            {
                short s = spec.Conditions[index, 0];
                short d = spec.Conditions[index, 1];

                AddItem(Conditions, s, d);
            }


            for (var index = 0; index < spec.Responses.GetLength(0); index++)
            {
                short s = spec.Responses[index, 0];
                short d = spec.Responses[index, 1];

                if (CheckItem(Conditions, d, s))
                {
                    RemoveItem(Conditions, d, s);
                    AddItem(ConditionResponses, s, d);
                }
                else
                {
                    AddItem(Responses, s, d);
                }
            }

            for (var index = 0; index < spec.Excludes.GetLength(0); index++)
            {
                short s = spec.Excludes[index, 0];
                short d = spec.Excludes[index, 1];

                if (CheckItem(Excludes, d, s))
                {
                    RemoveItem(Excludes, d, s);
                    AddItem(Mutexes, s, d);
                }
                else
                {
                    AddItem(Excludes, s, d);
                }
            }


            for (var index = 0; index < spec.Includes.GetLength(0); index++)
            {
                short s = spec.Includes[index, 0];
                short d = spec.Includes[index, 1];

                AddItem(Includes, s, d);
            }


        }

        private void AddItem(Dictionary<short, Dictionary<short, bool>> dict, short i, short j)
        {
            if (!dict.ContainsKey(i)) dict.Add(i, new Dictionary<short, bool>());
            if (!dict[i].ContainsKey(j)) dict[i].Add(j, true);
        }

        private void RemoveItem(Dictionary<short, Dictionary<short, bool>> dict, short i, short j)
        {
            dict[i].Remove(j);
        }

        private bool CheckItem(Dictionary<short, Dictionary<short, bool>> dict, short i, short j)
        {
            if (!dict.ContainsKey(i)) return false;
            return dict[i].ContainsKey(j);
        }

    }
}
