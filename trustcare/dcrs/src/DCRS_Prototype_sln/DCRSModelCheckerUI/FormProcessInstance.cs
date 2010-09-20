using System;
using System.Collections.Generic;
using System.Windows.Forms;
using DCRSModelCheckerUI.HelperClasses;
using DCRSModelCheckerUI.Services;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;
using ActionInfo = DCRSModelCheckerUI.HelperClasses.ActionInfo;

namespace DCRSModelCheckerUI
{
    public partial class FormProcessInstance : Form
    {
        public  DCRSProcess DCRSProcessInstance;

        public readonly int ProcessId;

        public readonly int ProcessInstanceId;

        public FormProcessInstance(int processId, int processInstanceId)
        {
            ProcessId = processId;

            ProcessInstanceId = processInstanceId;

            InitializeComponent();

            if ((processId <= 0) || (processInstanceId <= 0))
            {
                MessageBox.Show("Process Id and Process InstanceId are not specified!", "Invlid Information",
                                MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }

            // Get Xml form remote service handler.
            var processInstanceXml = RemoteServicesHandler.GetProcessInstance(processId, processInstanceId);

            DCRSProcessInstance = DCRSProcess.Deserialize(processInstanceXml);

            Name = string.Format("FormInstance{0}.{1}", ProcessId, ProcessInstanceId);

            // Load the process instance and populate the controls.
            LoadProcessInstance();


        }

        public FormProcessInstance(DCRSProcess dcrsProcessInstance)
        {
            this.DCRSProcessInstance = dcrsProcessInstance;

            ProcessId = dcrsProcessInstance.Specification.ProcessId;

            ProcessInstanceId = dcrsProcessInstance.Runtime.ProcessInstanceId;

            Name = string.Format("FormInstance{0}.{1}", ProcessId, ProcessInstanceId);
        }   


        
        public void LoadProcessInstance()
        {

            // Assign the Form title and group box titles.
            var formTitle = string.Format("Process Instance: {0} (ProcessId: {1}, InstanceId: {2}) ",
                                      DCRSProcessInstance.Specification.ModelName,
                                      DCRSProcessInstance.Specification.ProcessId,
                                      DCRSProcessInstance.Runtime.ProcessInstanceId);

            ThreadSafeCallHandler.SetControlText(this, formTitle);

            UpdateSpecificationFrame();

            UpdateRuntimeFrame();


        }

        private void UpdateRuntimeFrame()
        {

            // Update the heading for the group box for specification frame.
            var groupBoxText = string.Format("DCRS Process Instance: ProcessId = {0}, ProcessInstanceId = {1}",
                                                 DCRSProcessInstance.Specification.ProcessId,
                                                 DCRSProcessInstance.Runtime.ProcessInstanceId);

            ThreadSafeCallHandler.SetControlText(groupBoxRuntime, groupBoxText);



            Func<short, string> getName = (key) => DCRSProcessInstance.Specification.ActionList[key];


            ThreadSafeCallHandler.AddItemsToListControl(listBoxIncludedActions,
                                                        DCRSProcessInstance.Runtime.CurrentState.StateVector.
                                                            IncludedActions.ConvertAll(
                                                                new Converter<short, string>(getName)).ToArray());

            // executed actions
            ThreadSafeCallHandler.AddItemsToListControl(listBoxExecutedActions,
                                                        DCRSProcessInstance.Runtime.CurrentState.StateVector.
                                                            ExecutedActions.ConvertAll(
                                                                new Converter<short, string>(getName)).ToArray());



            //pending responses
            ThreadSafeCallHandler.AddItemsToListControl(listBoxPendingResponses,
                                                        DCRSProcessInstance.Runtime.CurrentState.StateVector.
                                                            PendingResponseActions.ConvertAll(
                                                                new Converter<short, string>(getName)).ToArray());

            
            ThreadSafeCallHandler.SetControlText(labelAceeptingState,
                                                 DCRSProcessInstance.Runtime.CurrentState.StateVector.StateAccepting.
                                                     ToString().ToLower());


            ThreadSafeCallHandler.SetControlText(labelStateRank,
                                                 DCRSProcessInstance.Runtime.CurrentState.StateVector.StateRank.ToString
                                                     ());


            Func<string, string> getNameById = (key) => (string.IsNullOrEmpty(key))
                                                            ? string.Empty
                                                            : DCRSProcessInstance.Specification.ActionList[
                                                                short.Parse(key)];

            var actionIds = DCRSProcessInstance.Runtime.ExecutionTrace.Split(new[] {','});

            var actionNamesArray = Array.ConvertAll(actionIds, new Converter<string, string>(getNameById));

            
            ThreadSafeCallHandler.SetControlText(textBoxExecutionTrace, string.Join(",", actionNamesArray));



            ThreadSafeCallHandler.UpdateListWithActionDictionaryValues(listBoxEnabledActions,
                                                                       Utilities.GetFilteredActionNamesDictionary(
                                                                           DCRSProcessInstance.Runtime.CurrentState.
                                                                               EnabledActions,
                                                                           DCRSProcessInstance.Specification.ActionList));  

            ThreadSafeCallHandler.AddItemsToListControl(listBoxPrincipalExe,
                                                        DCRSProcessInstance.Specification.Principals.ToArray());

        }


        private void UpdateSpecificationFrame()
        {

            // Update the heading for the group box for specification frame.
            var groupBoxSpecificationText = string.Format("DCRS Process Specification: Model name = {0}, ProcessId = {1}",
                                           DCRSProcessInstance.Specification.ModelName,
                                           DCRSProcessInstance.Specification.ProcessId);

            ThreadSafeCallHandler.SetControlText(groupBoxSpecification, groupBoxSpecificationText);

            // Assign the data to controls.
            //listBoxActionsSet.Items.AddRange(
            //    Utilities.ConvertDictionaryToActionInfoArray(dcrsProcessInstance.Specification.ActionList));

            ThreadSafeCallHandler.AddItemsToListControl(listBoxActionsSet,
                                                        Utilities.ConvertDictionaryToActionInfoArray(
                                                            DCRSProcessInstance.Specification.ActionList));

            ThreadSafeCallHandler.AddItemsToListControl(listBoxRoles, DCRSProcessInstance.Specification.Roles.ToArray());


            ThreadSafeCallHandler.AddItemsToListControl(listBoxPrincipals,
                                                        DCRSProcessInstance.Specification.Principals.ToArray());

            var itemsList = new List<ListViewItem>();

            foreach (var keyValuePair in DCRSProcessInstance.Specification.RolesToPrincipalsDictionary)
            {
                foreach (var subItem in keyValuePair.Value)
                {
                    var item = new ListViewItem { Text = keyValuePair.Key };

                    item.SubItems.Add(subItem);

                    itemsList.Add(item);
                }
            }

            // call thread safe 
            ThreadSafeCallHandler.AddItemsToListViewControl(listViewRoleToPrincipals, itemsList.ToArray());

            itemsList.Clear();

            //listViewRoleToPrincipals.Items.AddRange();
            foreach (var keyValuePair in DCRSProcessInstance.Specification.ActionsToRolesDictionary)
            {
                foreach (var subItem in keyValuePair.Value)
                {
                    var item = new ListViewItem { Text = DCRSProcessInstance.Specification.ActionList[keyValuePair.Key] };

                    item.SubItems.Add(subItem);

                    itemsList.Add(item);
                }
            }

            ThreadSafeCallHandler.AddItemsToListViewControl(listViewActionToRoles, itemsList.ToArray());

            // Clear off the listview controls.
            listViewIncludesExcludes.Items.Clear();

            listViewIncludesExcludes.Groups.Clear();

            listViewConditionResponses.Items.Clear();

            listViewConditionResponses.Groups.Clear();


            var group = new ListViewGroup("Include Relation");

            ListViewItem[] listViewItems = GetItemsForListViewWithGroups(DCRSProcessInstance.Specification.Includes,
                                                                           DCRSProcessInstance.Specification.ActionList, group);

            ThreadSafeCallHandler.AddItemsToListViewControlWithGroups(listViewIncludesExcludes, listViewItems, group);


            group = new ListViewGroup("Exclude Relation");

            listViewItems = GetItemsForListViewWithGroups(DCRSProcessInstance.Specification.Excludes,
                                                                           DCRSProcessInstance.Specification.ActionList, group);

            ThreadSafeCallHandler.AddItemsToListViewControlWithGroups(listViewIncludesExcludes, listViewItems, group);


            group = new ListViewGroup("Condition Relation");

            listViewItems = GetItemsForListViewWithGroups(DCRSProcessInstance.Specification.Conditions,
                                                                           DCRSProcessInstance.Specification.ActionList, group);

            ThreadSafeCallHandler.AddItemsToListViewControlWithGroups(listViewConditionResponses, listViewItems, group);



            group = new ListViewGroup("Strong Condition Relation");

            listViewItems = GetItemsForListViewWithGroups(DCRSProcessInstance.Specification.StrongConditions,
                                                                           DCRSProcessInstance.Specification.ActionList, group);

            ThreadSafeCallHandler.AddItemsToListViewControlWithGroups(listViewConditionResponses, listViewItems, group);

            group = new ListViewGroup("Response Relation");

            listViewItems = GetItemsForListViewWithGroups(DCRSProcessInstance.Specification.Responses,
                                                                           DCRSProcessInstance.Specification.ActionList, group);

            ThreadSafeCallHandler.AddItemsToListViewControlWithGroups(listViewConditionResponses, listViewItems, group);



        }


        private ListViewItem[] GetItemsForListViewWithGroups(short[,] relationArray, Dictionary<short, string> actionsLsit,
            ListViewGroup group)
        {
            var itemsList = new List<ListViewItem>();

            for (var index = 0; index < relationArray.GetLength(0); index++)
            {

                var item = new ListViewItem
                {
                    Text =
                        actionsLsit[relationArray[index, 0]],
                    Group = group
                };

                item.SubItems.Add(actionsLsit[
                    relationArray[index, 1]]);

                itemsList.Add(item);
            }

            return itemsList.ToArray();

        }

        private void AddRelationsToListViewWithGroups(short[,] relationArray, Dictionary<short, string> actionsLsit,
            string groupName, ListView listViewcontrol)
        {


            var @group = new ListViewGroup(groupName);

            listViewcontrol.Groups.Add(@group);

            for (var index = 0; index < relationArray.GetLength(0); index++)
            {

                var item = new ListViewItem
                {
                    Text =
                        actionsLsit[relationArray[index, 0]],
                    Group = @group
                };

                item.SubItems.Add(actionsLsit[
                    relationArray[index, 1]]);

                listViewcontrol.Items.Add(item);
            }



        }

        private void buttonExecuteAction_Click(object sender, EventArgs e)
        {
            if(listBoxEnabledActions.SelectedIndex == -1)
            {
                MessageBox.Show("Please select the action to execute.", "No action selected!", MessageBoxButtons.OK,
                                MessageBoxIcon.Information);
                return;
            }


            if (listBoxPrincipalExe.SelectedIndex == -1)
            {
                MessageBox.Show("Please select the necessary principal name to execute an action.", "No peincipal selected!", MessageBoxButtons.OK,
                                MessageBoxIcon.Information);
                return;
            }

            var actionId = ((ActionInfo) listBoxEnabledActions.SelectedItem).Id;

            var actionName = ((ActionInfo) listBoxEnabledActions.SelectedItem).Name;

            var principal = listBoxPrincipalExe.SelectedItem.ToString();

            var actionExecuteResult = RemoteServicesHandler.ExecuteAction(ProcessId, ProcessInstanceId, actionId, principal);

            ThreadSafeCallHandler.SetControlText(textBoxExeResult,
                                                 FormatExecutionMessage(actionName, principal, actionExecuteResult));


            //textBoxExeResult.Text = string.Format("Action execution status:{0}{1}{2}", actionExecuteResult.Status,
            //                                      Environment.NewLine, actionExecuteResult.Message);
        }

        private static string FormatExecutionMessage(string actionName, string principal, TaskResult result)
        {
            return result.Status
                       ? string.Format("Action: {0} with principal: {1} executed successfully!", actionName,
                                       principal)
                       : string.Format("Failed to execute action: {0} with principal: {1}! {2} {3}", actionName,
                                       principal, Environment.NewLine, result.Message);
        }
    }
}
