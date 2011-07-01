using System.Collections.Generic;
using System.Linq;

namespace ITU.DK.DCRS.CommonTypes.Process
{
    
    public class DCRSSpecification
    {

       
        // Rao: 20100713: To fix the multi dimensional arrays to jagged arrays
        // because of serialization problems, since it is a shame that .Net 3.5 can not serialize
        // multi dimesional arrays, where as it can serailize jagged arrays.

        #region Model name and process isntance-id

        public string ModelName;

        public int ProcessId;

        #endregion

        #region 1. Resources.

        public readonly Dictionary<short, string> ActionList;

        // (tijs) perhaps not the best method, but easiest for now:
        public readonly Dictionary<short, short> Nesting;

        public readonly List<string> Roles;

        public readonly List<string> Principals;

        #endregion

        #region 2. Access Controls

        public readonly Dictionary<string, List<string>> RolesToPrincipalsDictionary;

        public readonly Dictionary<short, List<string>> ActionsToRolesDictionary;

        #endregion

        #region 3. Constraints-sets.

        /// <summary>
        /// The first and second columns refere to Ids of actions in that relation.
        /// </summary>
        //public short[][] Includes;
        public short[,] Includes;
        public short[,] Excludes;
        public short[,] Responses;
        public short[,] Conditions;
        public short[,] StrongConditions;
        public short[,] Milestones;

        #endregion


        #region 4. Property-specification

        public readonly List<PropertyPattern> RuntimeMonitoringProperties;
        
        #endregion

        #region Constructor.

        public DCRSSpecification()
        {

            ModelName = string.Empty;

            ProcessId = -1;
            
            ActionList = new Dictionary<short, string>();

            Nesting = new Dictionary<short, short>();

            Roles = new List<string>();

            Principals = new List<string>();

            RolesToPrincipalsDictionary = new Dictionary<string, List<string>>();

            ActionsToRolesDictionary = new Dictionary<short, List<string>>();

            RuntimeMonitoringProperties = new List<PropertyPattern>();

            // Initialize constraint sets to 0 records, to prevent null exceptions.
            // If the user wants intialize they can assign required array.
            Includes = new short[0,2];
            Excludes = new short[0, 2];
            Responses = new short[0, 2];
            Conditions = new short[0, 2];
            StrongConditions = new short[0, 2];



        }

        #endregion


        #region Public Methods.

        public bool CanExecuteAction(string principal, short actionId)
        {

            var result = from actionRole in ActionsToRolesDictionary
                         where actionRole.Key == actionId
                         select actionRole.Value;

            var result2 = from rolesList in result
                          from role in rolesList
                          join roleToPrincipal in RolesToPrincipalsDictionary on
                              role equals roleToPrincipal.Key
                          where roleToPrincipal.Value.Contains(principal)
                          select roleToPrincipal.Value;

            return (result2.Count() > 0);
        }

        #endregion


    }
}
