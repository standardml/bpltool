using System.Collections.Generic;
using ITU.DK.DCRS.CommonTypes.Process;

namespace ITU.DK.DCRS.CommonTypes.Samples
{
    public class DCRSSampleModels
    {

        public static DCRSSpecification GetGiveMedicineSpecification()
        {


            var specification = new DCRSSpecification
                                    {
                                        ModelName = "GiveMedicineExample", 
                                        ProcessId = 5
                                    };

            #region 1. Resources.
            // Add roles
            specification.Roles.Add("doctor");

            specification.Roles.Add("nurse");

            // Add Principals..

            specification.Principals.Add("john");

            specification.Principals.Add("paul");

            specification.Principals.Add("anne");

            specification.Principals.Add("jasmine");

            // Add action lists.
            specification.ActionList.Add(0, "pm");

            specification.ActionList.Add(1, "s");

            specification.ActionList.Add(2, "gm");

            specification.ActionList.Add(3, "dt");


            
            #endregion

            #region 2. Access Controls.
            specification.RolesToPrincipalsDictionary.Add("doctor", new List<string> { "john", "paul" });

            specification.RolesToPrincipalsDictionary.Add("nurse", new List<string> { "anne", "jasmine" });


            specification.ActionsToRolesDictionary.Add(0, new List<string> { "doctor" });

            specification.ActionsToRolesDictionary.Add(1, new List<string> { "doctor" });

            specification.ActionsToRolesDictionary.Add(2, new List<string> { "nurse" });

            specification.ActionsToRolesDictionary.Add(3, new List<string> { "nurse" });
            
            #endregion            //specification.ActionsToRolesDictionary.Add("nurse", actionsNurse);

            #region 3. Constraint-sets

            var conditions = new short[3, 2] { { 1, 0 }, { 2, 1 }, { 3, 1 } };

            specification.Conditions = conditions;

            var responses = new short[3,2] {{0, 1}, {0, 2}, {3, 1}};

            specification.Responses = responses;

            var includes = new short[2, 2] { { 1, 2 }, { 1, 3 } };

            specification.Includes = includes;

            var excludes = new short[2, 2] { { 2, 3 }, { 3, 2 } };

            specification.Excludes = excludes;


            #endregion

            return specification;

        }




    }
}
