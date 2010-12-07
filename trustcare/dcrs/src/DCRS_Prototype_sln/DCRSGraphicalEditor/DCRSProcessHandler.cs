using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.Visualization.Elements;
using System.Data;
using ITU.DK.DCRS.WorkflowEngine.Core;


// reorder functions to make more sense at some point, create regions
namespace DCRSGraphicalEditor
{
    public enum PrimitiveType { Condition=0, Response, Include, Exclude };

    public class DCRSProcessHandler
    {
        DCRSProcess Process;

        //public DataSet DS;
        public DataTable Roles;
        public DataTable Principals;

        public DCRSProcessHandler(DCRSProcess p)
        {
            Process = p;
            DefineRoles();
            DefinePrincipals();
        }


        public void UpdateActionName(short id, string name)
        {
            Process.Specification.ActionList[id] = name;
        }


        public void IncludeAction(short id)
        {
            if (!Process.Runtime.CurrentState.StateVector.IncludedActions.Contains(id))
                Process.Runtime.CurrentState.StateVector.IncludedActions.Add(id);
            ComputeState();
        }

        public void ExcludeAction(short id)
        {
            if (Process.Runtime.CurrentState.StateVector.IncludedActions.Contains(id))
                Process.Runtime.CurrentState.StateVector.IncludedActions.Remove(id);
            ComputeState();
        }

        public void ClearActionRoles(short id)
        {
            Process.Specification.ActionsToRolesDictionary[id].Clear();
        }

        public void AddActionRole(short id, string r)
        {
            if (!Process.Specification.ActionsToRolesDictionary[id].Contains(r))
                Process.Specification.ActionsToRolesDictionary[id].Add(r);
        }

        public void RemoveActionRole(short id, string r)
        {
            if (Process.Specification.ActionsToRolesDictionary[id].Contains(r))
                Process.Specification.ActionsToRolesDictionary[id].Remove(r);
        }



        private void ComputeState()
        {
                var finiteStateProvider = new DCRSFiniteStateProvider(Process.Runtime.CurrentState.StateNumber,
                                                                      -1,
                                                                      Process.Runtime.CurrentState.StateVector,
                                                                      Process.Specification);

                var dcrsUpdatedState = finiteStateProvider.ComputeState();

                var executionTrace = (Process.Runtime.ExecutionTrace);
            
                // update the process instance with latest state and save it.
                Process.Runtime = new DCRSRuntime(dcrsUpdatedState, Process.Runtime.ProcessInstanceId, executionTrace);
        }

        private void DefineRoles()
        {
            Roles = new DataTable("Roles");
            Roles.Columns.Add("srcName", typeof(String));
            Roles.Columns.Add("Name", typeof(String));
            foreach (String a in Process.Specification.Roles)
                Roles.Rows.Add(new Object[] { a, a });

            Roles.RowDeleting += new DataRowChangeEventHandler(Roles_RowDeleting);
            Roles.RowChanged += new DataRowChangeEventHandler(Roles_RowChanged);
                       
            Roles.Constraints.Add(new UniqueConstraint(Roles.Columns[1]));
            Roles.Columns[1].AllowDBNull = false;
        }


        private void DefinePrincipals()
        {
            Principals = new DataTable("Principals");
            Principals.Columns.Add("srcName", typeof(String));
            Principals.Columns.Add("Name", typeof(String));

            Principals.Constraints.Add(new UniqueConstraint(Principals.Columns[1]));
            Principals.Columns[1].AllowDBNull = false;

            foreach (String a in Process.Specification.Roles)
            {
                Principals.Columns.Add("src_" + a, typeof(Boolean));
                Principals.Columns.Add(a, typeof(Boolean));
            }

            foreach (String a in Process.Specification.Principals)
            {
                Object[] row = new Object[(Process.Specification.Roles.Count + 1) * 2];

                row[0] = a;
                row[1] = a;
                int i = 2;
                foreach (String b in Process.Specification.Roles)
                {
                    var d = Process.Specification.RolesToPrincipalsDictionary[b];
                    row[i] = d.Contains(a);
                    i++;
                    row[i] = d.Contains(a);
                    i++;
                }
                Principals.Rows.Add(row);
            }
            Principals.RowDeleting += new DataRowChangeEventHandler(Principals_RowDeleting);
            Principals.RowChanged += new DataRowChangeEventHandler(Principals_RowChanged);
        }


        bool updating = false;
        void Roles_RowChanged(object sender, DataRowChangeEventArgs e)
        {
            if (updating) return;
            updating = true;
            String a = (String)e.Row[1];
            if (e.Action == DataRowAction.Change)
            {                
                String oldName = (String)e.Row[0];
                RenameRole(oldName, a);
                e.Row[0] = e.Row[1];
            }
            else if (e.Action == DataRowAction.Add)
            {                
                AddRole(a);
                e.Row[0] = e.Row[1];
            }
            updating = false;
        }

        void Roles_RowDeleting(object sender, DataRowChangeEventArgs e)
        {
            String a = e.Row.Field<String>(0);
            RemoveRole(a);
        }

        private void RemoveRole(string a, bool colremove = true)
        {
            Process.Specification.Roles.Remove(a);
            Process.Specification.RolesToPrincipalsDictionary.Remove(a);
            foreach (var v in Process.Specification.ActionsToRolesDictionary)
                if (v.Value.Contains(a)) v.Value.Remove(a);
            if (colremove)
            {
                Principals.Columns.Remove("src_" + a);
                Principals.Columns.Remove(a);
            }
        }


        private void AddRole(string a)
        {
            Process.Specification.Roles.Add(a);
            Process.Specification.RolesToPrincipalsDictionary.Add(a, new List<string>());
            Principals.Columns.Add("src_" + a, typeof(Boolean));
            Principals.Columns.Add(a, typeof(Boolean));
        }


        private void RenameRole(string o, string n)
        {
            Process.Specification.Roles.Add(n);
            Process.Specification.RolesToPrincipalsDictionary.Add(n, Process.Specification.RolesToPrincipalsDictionary[o]);
            foreach (var v in Process.Specification.ActionsToRolesDictionary)
                if (v.Value.Contains(o)) v.Value.Add(n);
            RemoveRole(o, false);
            Principals.Columns[Principals.Columns.IndexOf("src_" + o)].ColumnName = "src_" + n;
            Principals.Columns[Principals.Columns.IndexOf(o)].ColumnName = n;
        }




        bool Principalsupdating = false;
        void Principals_RowChanged(object sender, DataRowChangeEventArgs e)
        {
            if (Principalsupdating) return;
            Principalsupdating = true;
            String a = (String)e.Row[1];
            if (e.Action == DataRowAction.Change)
            {
                String oldName = (String)e.Row[0];
                if (oldName != a)
                {                    
                    RenamePrincipal(oldName, a);
                    e.Row[0] = e.Row[1];
                }
                // do role assignment changes
                for (int i = 0; i < Principals.Columns.Count; i = i + 2)
                {
                    if (e.Row[i] != e.Row[i + 1])
                    {
                        //update
                        string role = Principals.Columns[i + 1].ColumnName;// role name
                        string principal = a;
                        if ((bool)e.Row[i + 1])
                            AddPrincipalToRole(role, principal);
                        else
                            RemovePrincipalFromRole(role, principal);

                        e.Row[i] = e.Row[i + 1];
                    }
                }

            }
            else if (e.Action == DataRowAction.Add)
            {
                AddPrincipal(a);
                e.Row[0] = e.Row[1];
                // do role assignments?
            }
            Principalsupdating = false;
        }

        void Principals_RowDeleting(object sender, DataRowChangeEventArgs e)
        {
            String a = e.Row.Field<String>(0);
            RemovePrincipal(a);
        }

        private void RemovePrincipal(string a)
        {
            Process.Specification.Principals.Remove(a);
            foreach (var d in Process.Specification.RolesToPrincipalsDictionary)
                if (d.Value.Contains(a)) d.Value.Remove(a);            
        }


        private void AddPrincipal(string a)
        {
            Process.Specification.Principals.Add(a);
        }


        private void RenamePrincipal(string o, string n)
        {
            Process.Specification.Principals.Add(n);
            foreach (var d in Process.Specification.RolesToPrincipalsDictionary)
                if (d.Value.Contains(o)) d.Value.Add(n);
            RemovePrincipal(o);
        }

        private void AddPrincipalToRole(string r, string p)
        {
            if (!Process.Specification.RolesToPrincipalsDictionary[r].Contains(p))
                Process.Specification.RolesToPrincipalsDictionary[r].Add(p);
        }


        private void RemovePrincipalFromRole(string r, string p)
        {
            if (Process.Specification.RolesToPrincipalsDictionary[r].Contains(p))
                Process.Specification.RolesToPrincipalsDictionary[r].Remove(p);
        }


        public short AddAction()
        {
            short maxNode;
            if (Process.Specification.ActionList.Count == 0)
                maxNode = -1;
            else
                maxNode = Process.Specification.ActionList.Max(x => x.Key);
            short newNode = (short)(maxNode + 1);
            Process.Specification.ActionList.Add(newNode, "Event " + newNode.ToString());
            Process.Specification.ActionsToRolesDictionary.Add(newNode, new List<string>());
            Process.Runtime.CurrentState.StateVector.IncludedActions.Add(newNode); // auto include
            
            ComputeState();
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

            ComputeState();
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
            
            ComputeState();
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
            
            ComputeState();
        }
    }
}
