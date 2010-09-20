using System;
using System.Collections.Generic;
using System.Linq;
using ITU.DK.DCRS.CommonTypes.Process;

namespace ITU.DK.DCRS.CommonTypes
{
    public static class Utilities
    {
        public const char TAB = (char)9;

        public const short TAU_ACTION = short.MaxValue;

        public const string TAU_ACTION_NAME = "TAU";


        public static short? GetValueFromTwoDimArray(short[,] array, short key)
        {
            for (int index = 0; index < array.GetLength(0); index++)
            {
                if(array[index,0] == key)
                {
                    return array[index, 1];
                }
            }

            return null;
        }


        public static short? GetKeyFromTwoDimArray(short[,] array, short value)
        {
            for (int index = 0; index < array.GetLength(0); index++)
            {
                if (array[index, 1] == value)
                {
                    return array[index, 0];
                }
            }

            return null;
        }

        public static ActionInfo[] ConvertDictionaryToActionInfoArray(Dictionary<short, string > actionsDictionary )
        {
            return actionsDictionary.Select(keyValuePair => new ActionInfo {ActionId = keyValuePair.Key, Name = keyValuePair.Value}).ToList().ToArray();
        }


        public static string [] GetActionNames(List<short> actionList, Dictionary<short ,string > actionDictionary)
        {
            
            Func<short, string> getName = (key) => actionDictionary[key];

            return actionList.ConvertAll(new Converter<short, string>(getName)).ToArray();

        }


        public static Dictionary<short, string> GetFilteredActionNamesDictionary(List<short> actionList, Dictionary<short, string> actionDictionary)
        {

            Func<short, string> getName = (key) => actionDictionary[key];

            var filteredActionNamesDictionary = actionList.ToDictionary(key => key, getName);

            return filteredActionNamesDictionary;

        }


        public static string GetTransitionLabel(Dictionary<short, string> actionLabels, short transition)
        {

            if (transition == -1) return string.Empty;

            return transition == TAU_ACTION ? TAU_ACTION_NAME : actionLabels[transition];
        }




    }
}
