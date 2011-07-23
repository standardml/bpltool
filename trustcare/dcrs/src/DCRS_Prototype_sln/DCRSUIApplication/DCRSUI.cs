using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Windows.Forms;
using System.Xml;
using System.Xml.Linq;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.CommonTypes.Examples;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.CommonTypes.Samples;
using ITU.DK.DCRS.CommonTypes.Serialization;
using ITU.DK.DCRS.WorkflowEngine.Core;
using ITU.DK.DCRS.WorkflowEngine.DataAccess;
using DCRSToProMeLaCompiler;
using DCRStoFiniteAutomaton;
using Microsoft.Glee.Drawing;

namespace DCRSUIApplication
{
    public partial class DCRSUI : Form
    {
        public DCRSUI()
        {
            InitializeComponent();
        }

        private void datagridactions_RowEnter(object sender, DataGridViewCellEventArgs e)
        {
            datagridactions.Rows[e.RowIndex].Cells[0].Value = e.RowIndex;

            datagridactions.Rows[e.RowIndex].Cells[2].Value = 1;


        }



        private void buttonGenPromelaCode_Click(object sender, EventArgs e)
        {
            try
            {

                if (!CheckDataValidatiy()) return;


                if (string.IsNullOrEmpty(textBoxPromelaCodePath.Text))
                {
                    MessageBox.Show("Select the folder path!", "Invalid Folder Path!", MessageBoxButtons.OK, MessageBoxIcon.Warning);

                    return;
                }


                //var actionsList = TransformActionsGrid(datagridactions);

                //var includes = TransformRelations(datagrid_includes);

                //var excludes = TransformRelations(datagrid_excludes);

                //var responses = TransformRelations(datagrid_response);

                //var conditions = TransformRelations(datagrid_condition);

                //var initial_state = TransformInitialState(datagridactions);

                //var dcrModel = new DCRSModel(actionsList, includes, excludes, responses, conditions,
                //                             textBoxModelName.Text) {InitialState = initial_state};



                var dcrModel = GetDCRSModel();


                var filePath = DCRSCompiler.ComplileDcrsModelForStrongAcceptanceCondition(dcrModel, textBoxPromelaCodePath.Text);

                Process.Start(filePath);




            }
            catch (Exception ex)
            {

                MessageBox.Show(ex.Message, "Exception Occured", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

        }

        private void buttonSelectPath1_Click(object sender, EventArgs e)
        {

            if(folderBrowserDialog1.ShowDialog() == DialogResult.OK)
            {
                textBoxPromelaCodePath.Text = folderBrowserDialog1.SelectedPath;

            }



        }



        private void buttonTest_Click(object sender, EventArgs e)
        {
            SampleDCRSExamples.GetGiveMedicineSmall();

        }

        private void buttonPromelaCodeWeaker_Click(object sender, EventArgs e)
        {
            try
            {

                if (!CheckDataValidatiy()) return;

                if (string.IsNullOrEmpty(textBoxModelName.Text))
                {
                    MessageBox.Show("Please Enter Model Name!", "Model Name empty!", MessageBoxButtons.OK, MessageBoxIcon.Warning);

                    return;
                }


                if (string.IsNullOrEmpty(textBoxPromelaCodePath.Text))
                {
                    MessageBox.Show("Select the folder path!", "Invalid Folder Path!", MessageBoxButtons.OK, MessageBoxIcon.Warning);

                    return;
                }


                var actionsList = TransformActionsGrid(datagridactions);

                var includes = TransformRelations(datagrid_includes);

                var excludes = TransformRelations(datagrid_excludes);

                var responses = TransformRelations(datagrid_response);

                var conditions = TransformRelations(datagrid_condition);

                var initial_state = TransformInitialState(datagridactions);

                var dcrModel = new DCRSModel(textBoxModelName.Text, actionsList, includes, excludes, responses, conditions
                                             ) { InitialIncludedActions = initial_state };


                var filePath = DCRSCompiler.ComplileDcrsModelWeakerAcceptanceCondition(dcrModel, textBoxPromelaCodePath.Text);

                System.Diagnostics.Process.Start(filePath);




            }
            catch (Exception ex)
            {

                MessageBox.Show(ex.Message, "Exception Occured", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

        }

        private static void PrintList(List<short> list)
        {


            for (var index = 0; index < list.Count; index++)
            {
                Console.WriteLine(list[index]);
            }


        }

        private void button2_Click(object sender, EventArgs e)
        {
            
            //if(string.IsNullOrEmpty(textBoxStatespace.Text))
            //{

            //    MessageBox.Show("Select the folder path!", "Invalid Folder Path!", MessageBoxButtons.OK, MessageBoxIcon.Warning);

            //    return;

            //}


            //var autamatonSettings = new AutamatonSettings
            //                            {StateMode = AutomatonMode.FiniteSate, ExcludeNonConditionalEvents = true};

            //var autamatonSettings = new AutamatonSettings { StateMode = AutomatonMode.Buchi, ExcludeNonConditionalEvents = false, IncludeTauAction = false};

            //var stateManager = StateManager.GetStateManagerInstance();

            //stateManager.Settings = autamatonSettings;

            //stateManager.Specification = DCRSExamples.GetTwoSelfResponseEventsExample();
            //stateManager.Specification = DCRSExamples.GetGiveMedicineFullExample();

            //stateManager.Specification = DCRSExamples.GetGiveMedicineSmallExample();



            //stateManager.Specification = DCRSExamples.GetGiveMedicineSmallExampleWithTauAction();

            //stateManager.Specification = DCRSExamples.GetArrangeMeetingSampleShort();

            //stateManager.Specification = DCRSExamples.GetArrangeMeetingSampleShortWithCreateCase();

            var autamatonSettings = new AutamatonSettings { StateMode = AutomatonMode.FiniteSate, ExcludeNonConditionalEvents = false, IncludeTauAction = false };

            var stateManager = StateManager.GetStateManagerInstance();

            stateManager.Settings = autamatonSettings;

            //stateManager.Specification = DCRSExamples.GetGiveMedicineSmallExample();

            //stateManager.Specification = DCRGSamples.GetCaseHandlingDCRGProcess();
            stateManager.Specification = DCRGSamples.GetCursePrayExampleStrict();

            
            //stateManager.Specification = DCRSExamples.BuchiToDCRTranslationsExample2();

            

            Dictionary<long,AtomicState> stateSpace = stateManager.ComputeStateSpace();


            if (!string.IsNullOrEmpty(textBoxStatespace.Text))
            {
                string path = string.Format(@"{0}\{1}.csv", textBoxStatespace.Text, stateManager.Specification.ModelName);

                StreamWriter writer = new StreamWriter(path);

                writer.WriteLine("StateNumber;LeadingTransition;ExecutedEvents;IncludedEvents;PendingResponseEvents;IncludedPendingResponseEvents;StateRank;HigherPendingResponseEvents;StateAccepting;EnabledTransitions;Transitions;");


                foreach (var keyvalPair in stateSpace)
                {
                    writer.WriteLine(keyvalPair.Value);

                }

                writer.Flush();

                writer.Close();
                
            }



            Graph graph = new Graph("graph");

            graph.BuildNodeHierarchy = true;

            

            foreach (var keyValPair in stateSpace)
            {
                string nodeId = string.Format("s{0}", keyValPair.Value.StateNumber);

                var node = graph.AddNode(nodeId);

                // Node attributes
                if (keyValPair.Value.StateVector.StateAccepting) 
                {
                    node.Attr.Shape = Shape.DoubleCircle;

                    node.Attr.Fillcolor = new Color(204,255,153);

                }
                
                foreach (Transition transition in keyValPair.Value.Transitions)
                {
                    if(transition.Direction == TransitionDirection.Incoming)
                    {
                        //graph.AddEdge(transition.StateNumber.ToString(),
                        //              keyValPair.Value.LeadingTransition.ToString(),
                        //              keyValPair.Value.StateNumber.ToString());
                    }
                    else
                    {
                        //var edgelabel =  stateManager.Specification.ActionList[transition.Label];
                        var edgelabel = Utilities.GetTransitionLabel(stateManager.Specification.ActionList,
                                                                     transition.Label);
                        var targetNodeId = string.Format("s{0}", transition.StateNumber);

                        

                        //node.AddOutEdge();
                        var edge = graph.AddEdge(nodeId, edgelabel, targetNodeId);

                        //if (edgelabel.EndsWith("DA"))
                        //{
                        //    edge.EdgeAttr.AddStyle(Style.Dotted);

                        //    edge.Attr.Color = Color.Red;
                        //}
                        //else
                        //{
                        //    edge.Attr.Color = Color.Green;

                        //}

                        edge.Attr.LineWidth = 2;

                        

                        node.AddOutEdge(edge);

                       
                        
                        //graph.AddEdge(nodeId,
                        //             edgelabel,
                        //              transition.StateNumber.ToString());

                    }
                }

            }


           
            graph.GraphAttr.NodeAttr.Padding = 3;
            //Edge edge = (Edge)g.AddEdge("S24", "27");
            //edge.Attr.Label = "Edge Label Test";

            gViewer.Graph = graph;
            
            //gViewer.






        }

        private void button3_Click(object sender, EventArgs e)
        {
            if (folderBrowserDialog1.ShowDialog() == DialogResult.OK)
            {
                textBoxStatespace.Text = folderBrowserDialog1.SelectedPath;

            }

        }

        private void buttonTest_Click_1(object sender, EventArgs e)
        {
            try
            {

                // string result = DCRSCompiler.ComplileDcrsModelForStrongAcceptanceCondition(model, @"D:\PhDWork\Temp");
                
                var dcrsModel = SampleDCRSExamples.GetGiveMedicineSmall();

                var serializeDCRSModel = DCRSModelSerializer.SerializeDCRSModel(dcrsModel);

                var deSerializeDCRSModel = DCRSModelSerializer.DeSerializeDCRSModel(serializeDCRSModel);

                MessageBox.Show(serializeDCRSModel);

            }
            catch (Exception exception)
            {

                MessageBox.Show(exception.Message);
            }
        }

        private void button1_Click_1(object sender, EventArgs e)
        {
            var actions = new int[] { 2, 3, 5, 7, 8 };

            var temp = new int[5];

            temp[0] = 4;

            temp[1] = 3;

            temp[2] = 5;

            temp.Contains(0);

            var boolArray = new object[3];

            Console.WriteLine(actions.Contains(5));

            var sampleSet = new List<short> { 1, 11, 4, 2, 7, 9, 5 };

            sampleSet.Add(15);

            sampleSet.Add(19);

            sampleSet.Sort();

            sampleSet.Sort();

            PrintList(sampleSet);



            //if (sampleSet.Contains(9)) sampleSet.Remove(9);

            var newList = new List<short>();

            newList.AddRange(sampleSet.ToArray());

            newList.Add(6);

            newList.Add(13);

            Console.WriteLine("List before removal");

            PrintList(newList);

            foreach (short s in newList.ToArray())
            {
                if (sampleSet.Contains(s))
                    newList.Remove(s);

            }

            //for (int index = 0; index < newList.Count; index++)
            //{
            //    if(sampleSet.Contains(newList[index]))
            //        newList.Remove(newList[index]);

            //}

            Console.WriteLine("List after removal");

            PrintList(newList);

            



        }

        private void button4_Click(object sender, EventArgs e)
        {

            try
            {

                // Use ProcessStartInfo class
                ProcessStartInfo startInfo = new ProcessStartInfo();
                startInfo.CreateNoWindow = false;
                startInfo.UseShellExecute = false;
                startInfo.FileName = "zc.exe";
                startInfo.WindowStyle = ProcessWindowStyle.Normal;
                startInfo.RedirectStandardOutput = true;
                startInfo.RedirectStandardError = true;
                startInfo.Arguments = @"C:\PhDWork\ZingDCRS\DCRSSample.zing1.1.cs";
                using (Process process = Process.Start(startInfo))
                {
                    //
                    // Read in all the text from the process with the StreamReader.
                    //
                    using (StreamReader reader = process.StandardOutput)
                    {
                        string result = reader.ReadToEnd();
                        
                        Console.Write(result);

                        MessageBox.Show(result);
                    }
                }


                ProcessStartInfo startInfo2 = new ProcessStartInfo();
                startInfo2.CreateNoWindow = false;
                startInfo2.UseShellExecute = false;
                startInfo2.FileName = "zinger.exe";
                startInfo2.WindowStyle = ProcessWindowStyle.Normal;
                startInfo2.RedirectStandardOutput = true;
                startInfo2.RedirectStandardError = true;
                startInfo2.Arguments = @"C:\PhDWork\ZingDCRS\DCRSSample.zing1.1.dll";


                using (Process process = Process.Start(startInfo2))
                {
                    //
                    // Read in all the text from the process with the StreamReader.
                    //
                    using (StreamReader reader = process.StandardOutput)
                    {
                        string result = reader.ReadToEnd();

                        Console.Write(result);

                        MessageBox.Show(result);
                    }
                }




            }
            catch (Exception exception)
            {

                MessageBox.Show(exception.Message);
            }

        }

        private void buttonZingFiniteRuns_Click(object sender, EventArgs e)
        {





        }




        #region DCRS model helper functions.

        #region Transform the Datagrids


        private static Dictionary<short, string> TransformActionsGrid(DataGridView dataGridView)
        {

            var actionsList = new Dictionary<short, string>();

            foreach (DataGridViewRow row in dataGridView.Rows)
            {
                if ((row.Cells[0].Value != null) && (row.Cells[1].Value != null))
                    actionsList.Add(Convert.ToInt16(row.Cells[0].Value), row.Cells[1].Value.ToString());
            }

            return actionsList;
        }

        private static short[,] TransformRelations(DataGridView dataGridView)
        {
            var relationArray = new short[NonEmptyRows(dataGridView), 2];

            for (int index = 0; index < dataGridView.Rows.Count; index++)
            {
                if ((dataGridView.Rows[index].Cells[0].Value != null) && (dataGridView.Rows[index].Cells[1].Value != null))
                {
                    relationArray[index, 0] = Convert.ToInt16(dataGridView.Rows[index].Cells[0].Value);

                    relationArray[index, 1] = Convert.ToInt16(dataGridView.Rows[index].Cells[1].Value);
                }

            }

            return relationArray;
        }

        private static short NonEmptyRows(DataGridView dataGridView)
        {
            short index = 0;
            foreach (DataGridViewRow row in dataGridView.Rows)
            {
                if ((row.Cells[0].Value != null) && (row.Cells[1].Value != null)) index++;
            }
            return index;

        }

        private static short[,] TransformInitialState(DataGridView dataGridView)
        {

            var intialState_actions = new short[NonEmptyRows(dataGridView), 2];

            for (int index = 0; index < dataGridView.Rows.Count; index++)
            {
                if (dataGridView.Rows[index].Cells[0].Value != null)
                {
                    intialState_actions[index, 0] = Convert.ToInt16(dataGridView.Rows[index].Cells[0].Value);

                    // Convert.ToInt16(dataGridView.Rows[index].Cells[2].Value)
                    intialState_actions[index, 1] = (short)((dataGridView.Rows[index].Cells[2].Value.ToString() == "1")
                                                                 ? 1
                                                                 : 0);
                }

            }

            return intialState_actions;

        }




        #endregion

        private bool CheckDataValidatiy()
        {

            if (string.IsNullOrEmpty(textBoxModelName.Text))
            {
                MessageBox.Show("Please Enter Model Name!", "Model Name empty!", MessageBoxButtons.OK, MessageBoxIcon.Warning);

                return false;
            }


            if (datagridactions.Rows.Count <= 1)
            {
                MessageBox.Show("Not enough actions specified in the model", "Too Few Actions",
                    MessageBoxButtons.OK, MessageBoxIcon.Warning);
                return false;
            }

            int max_number_actions = NonEmptyRows(datagridactions);

            foreach (DataGridViewRow row in datagridactions.Rows)
            {
                if ((row.Cells[1].Value == null) || (string.IsNullOrEmpty(row.Cells[1].Value.ToString()))) continue;

                if (!row.Cells[1].Value.ToString().Trim().Contains(" ")) continue;
                MessageBox.Show(
                    string.Format("Spaces are not allowed in action names! Action name: {0} contains space",
                                  row.Cells[1].Value), "Invalid action name!",
                    MessageBoxButtons.OK, MessageBoxIcon.Warning);

                return false;
            }

            foreach (DataGridViewRow row in datagrid_includes.Rows)
            {
                if (!CheckRowData(row, "include", max_number_actions))
                {
                    return false;
                }
            }

            foreach (DataGridViewRow row in datagrid_excludes.Rows)
            {
                if (!CheckRowData(row, "exclude", max_number_actions))
                {
                    return false;
                }
            }

            foreach (DataGridViewRow row in datagrid_condition.Rows)
            {
                if (!CheckRowData(row, "condition", max_number_actions))
                {
                    return false;
                }
            }

            foreach (DataGridViewRow row in datagrid_response.Rows)
            {
                if (!CheckRowData(row, "response", max_number_actions))
                {
                    return false;
                }
            }


            return true;

        }

        private static bool CheckRowData(DataGridViewRow row, string relationName, int max_number_actions)
        {
            short numericValue;

            // This will be dummy empty row at the bottom.
            if ((row.Cells[0].Value == null) && (row.Cells[1].Value == null))
                return true;

            if ((row.Cells[0].Value == null) || (row.Cells[1].Value == null))
            {
                MessageBox.Show(
                    string.Format(
                        "Some of the {0} relations are not valid! A {0} relation is binary and it must have both parent and child ids.",
                        relationName),
                    "Invalid relation!",
                    MessageBoxButtons.OK, MessageBoxIcon.Warning);

                return false;
            }


            if (!short.TryParse(row.Cells[0].Value.ToString(), out numericValue))
            {
                MessageBox.Show(
                string.Format("The value: {0} is not a numeric value! Please enter a valid action Id",
                              row.Cells[0].Value), "Invalid action Id!",
                MessageBoxButtons.OK, MessageBoxIcon.Warning);

                return false;
            }

            if (numericValue >= max_number_actions)
            {
                MessageBox.Show(
                    string.Format("The value: {0} is not a valid action Id! Please enter a valid action Id",
                                  numericValue), "Invalid action Id!",
                    MessageBoxButtons.OK, MessageBoxIcon.Warning);
                return false;
            }


            if (!short.TryParse(row.Cells[1].Value.ToString(), out numericValue))
            {
                MessageBox.Show(
                string.Format("The value: {0} is not a numeric value! Please enter a valid action Id",
                              row.Cells[1].Value), "Invalid action Id!",
                MessageBoxButtons.OK, MessageBoxIcon.Warning);

            }

            if (numericValue >= max_number_actions)
            {
                MessageBox.Show(
                    string.Format("The value: {0} is not a valid action Id! Please enter a valid action Id",
                                  numericValue), "Invalid action Id!",
                    MessageBoxButtons.OK, MessageBoxIcon.Warning);
                return false;
            }



            return true;


        }

        private DCRSModel GetDCRSModel()
        {

            var actionsList = TransformActionsGrid(datagridactions);

            var includes = TransformRelations(datagrid_includes);

            var excludes = TransformRelations(datagrid_excludes);

            var responses = TransformRelations(datagrid_response);

            var conditions = TransformRelations(datagrid_condition);

            var milestones = TransformRelations(datagrid_milestones);

            var strongconditions = new short[0, 2];

            var initialState = TransformInitialState(datagridactions);

            var dcrModel = new DCRSModel(textBoxModelName.Text, actionsList, includes, excludes, responses, conditions,strongconditions, milestones
                                         ) { InitialIncludedActions = initialState };


            return dcrModel;
            
        }





        #endregion

        private void buttonDCRSPathSelector_Click(object sender, EventArgs e)
        {
            openFileDialog1.Filter = "Xml Files|*.xml|All Files|*.*";

            
            if(openFileDialog1.ShowDialog() == DialogResult.OK)
            {
                textBoxDCRSLoadpath.Text = openFileDialog1.FileName;
            } 
        }

        private void buttonDCRSSavePath_Click(object sender, EventArgs e)
        {
            saveFileDialog1.Filter = "Xml Files|*.xml|All Files|*.*";

            if(saveFileDialog1.ShowDialog() == DialogResult.OK)
            {
                textBoxDCRSSavePath.Text = saveFileDialog1.FileName;
            }

        }

        private void buttonDCRSSave_Click(object sender, EventArgs e)
        {
            if(!CheckDataValidatiy()) return;

            if (string.IsNullOrEmpty(textBoxDCRSSavePath.Text))
            {
                MessageBox.Show("Please select the save path!", "Save path not specified", MessageBoxButtons.OK,
                                MessageBoxIcon.Exclamation);
                return;
                
            }
            try
            {
                // First Get the DCRS model from the GUI.
                var dcrsModel = GetDCRSModel();

                var serializedDCRSXml = DCRSModelSerializer.SerializeDCRSModel(dcrsModel);

                var element = XElement.Load(new StringReader(serializedDCRSXml));

                element.Save(textBoxDCRSSavePath.Text);


            }
            catch (Exception exception)
            {

                MessageBox.Show(exception.Message, "Failed to save DCRS Model", MessageBoxButtons.OK,
                                MessageBoxIcon.Error);
            }

        }
        
        //delegate string GetXml(string path);


        private void buttonDCRSLoad_Click(object sender, EventArgs e)
        {

            if (string.IsNullOrEmpty(textBoxDCRSLoadpath.Text))
            {
                MessageBox.Show("Please select the DCRS Xml file to load!", "Path not specified", MessageBoxButtons.OK,
                                MessageBoxIcon.Exclamation);
                return;
            }

            string dcrsXml = string.Empty;

            Func<string, string> LoadXml = (path) =>
                                                {
                                                    for (int index = 0; index < 10; index++)
                                                    {
                                                        

                                                    }
                                                    var doc = new XmlDocument();
                                                    doc.Load(path);
                                                    return doc.OuterXml;

                                                }; 



            try
            {
                dcrsXml = LoadXml(textBoxDCRSLoadpath.Text);

            }
            catch (Exception exception)
            {

                MessageBox.Show(exception.Message, "Failed to load DCRS Model", MessageBoxButtons.OK,
                                MessageBoxIcon.Error);
                return;
            }

            var deSerializedDCRSModel = DCRSModelSerializer.DeSerializeDCRSModel(dcrsXml);

            textBoxModelName.Text = deSerializedDCRSModel.ModelName;

            // Clear off the old data if any.
            datagridactions.Rows.Clear();

            datagrid_condition.Rows.Clear();

            datagrid_response.Rows.Clear();

            datagrid_milestones.Rows.Clear();

            datagrid_includes.Rows.Clear();

            datagrid_excludes.Rows.Clear();



            foreach (var keyValuePair in deSerializedDCRSModel.ActionList)
            {
                datagridactions.Rows.Add(keyValuePair.Key, keyValuePair.Value, 1);
            }

            // Populate Relations Array
            Func<DataGridView, short[,], DataGridView> FillRelations = (DataGridView datagrid, short[,] array) =>
                                     {
                                         for (int index = 0; index < array.GetLength(0); index++)
                                         {
                                             datagrid.Rows.Add(array[index, 0], array[index, 1]);
                                         }

                                         return datagrid;
                                     };

            FillRelations(datagrid_condition, deSerializedDCRSModel.Conditions);

            FillRelations(datagrid_milestones, deSerializedDCRSModel.MileStones);

            FillRelations(datagrid_response, deSerializedDCRSModel.Responses);

            FillRelations(datagrid_includes, deSerializedDCRSModel.Includes);

            FillRelations(datagrid_excludes, deSerializedDCRSModel.Excludes);


        }

        private void button5_Click(object sender, EventArgs e)
        {

            try
            {
                var specification = DCRSSampleModels.GetGiveMedicineSpecification();

                var canExecuteAction = specification.CanExecuteAction("anne", 5);

                var serializedXml = DCRSSpecificationSerializer.SerializeDCRSSpecificationToString(specification);

                var specification1 = DCRSSpecificationSerializer.DeserializeDCRSSpecificationXml(serializedXml);
                
                
                var path = @"C:\PhDWork\dcrs documents\GiveMedicineInDCRSv1.5.xml";

                

                SaveXml(serializedXml, path);

                Process.Start(path);

                MessageBox.Show("done");
            }
            catch (Exception exception)
            {

                MessageBox.Show(exception.Message);
            }
        }


        public readonly Func<string, string, bool> SaveXml = ( xml, path) =>
                                                         {
                                                             var reader = new StringReader(xml);

                                                             var rootElement = XElement.Load(reader);

                                                             rootElement.Save(path);

                                                             return true;
                                                         };

        private void button6_Click(object sender, EventArgs e)
        {


            try
            {
                var specification = DCRSSampleModels.GetGiveMedicineSpecification();

                specification.ProcessId = 6;

                // Parent State Vector (S0)
                var parentVector = new DCRSStateVector();

                parentVector.IncludedActions.Add(0);
                parentVector.IncludedActions.Add(1);
                parentVector.IncludedActions.Add(2);
                parentVector.IncludedActions.Add(3);

                // Current state.
                var atomicStateProvider = new DCRSFiniteStateProvider(-1, -1, parentVector, specification);

                var runtime = new DCRSRuntime(atomicStateProvider.ComputeState(), 5, string.Empty);

                var serializeDCRSSpecificationToString = DCRSRuntimeSerializer.SerializeDCRSRuntimeToString(runtime);

                var dcrsRuntime = DCRSRuntimeSerializer.DeserializeDCRSRuntimeXml(serializeDCRSSpecificationToString);

                var dcrsProcess = new DCRSProcess() {Specification = specification, Runtime = runtime};

                DCRSWorkflowEngine.ProcessRepositoryProvider.SaveProcess(dcrsProcess);

                DCRSWorkflowEngine.ProcessRepositoryProvider.SaveProcessInstance(dcrsProcess);

                //var filePath = string.Format(@"{0}\{1}_{2}_{3}.xml", @"C:\PhDWork\dcrsdocuments", dcrsProcess.Specification.ModelName,
                //                             dcrsProcess.Specification.ProcessId,
                //                             dcrsProcess.Runtime.ProcessInstanceId);


                //DCRSProcess.Save(dcrsProcess, filePath);

                var dirPath = @"C:\PhDWork\DCRSRepository\Specifications";

                DirectoryInfo directoryInfo = new DirectoryInfo(dirPath);

               


                MessageBox.Show(serializeDCRSSpecificationToString);
            }
            catch (Exception exception)
            {
                
                MessageBox.Show(exception.Message);
            }
        }

        private void button7_Click(object sender, EventArgs e)
        {
            var processList = DCRSWorkflowEngine.ProcessRepositoryProvider.GetProcessList();


            var dirPath = @"C:\PhDWork\DCRSRepository\ProcessInstances";

            //Regex regex = new Regex("\\((?<TextInsideBrackets>\\w+)\\)");

            Regex regex = new Regex("\\((?<aTextInsideBrackets>\\w+)\\)");
            // \((?<aTextInsideBrackets>\w+)\)

            var regex1 = new Regex(@"pid#(?<aTextInsideBrackets>\w+)#");

            //"C:\PhDWork\DCRSRepository\ProcessInstances\(GiveMedicineExample).[pid#5#].[iid#55#].Xml"
            var regex2 = new Regex(@"iid#(?<aTextInsideBrackets>\w+)#");

            /* 
             \((?<TextInsideBrackets>\w+)\)
             */

            //string incomingValue = "Username (sales)";
            //string insideBrackets = null;
            //Match match = regex.Match(incomingValue);


            foreach (string fileName in Directory.GetFiles(dirPath))
            {
                Console.WriteLine(fileName);
                // C:\PhDWork\DCRSRepository\Specifications\(GiveMedicineExample).[pid#5#].Xml
                var match = regex.Match(fileName);

                if (match.Success)
                {
                    Console.WriteLine(match.Groups["aTextInsideBrackets"].Value);
                }

                var match1 = regex1.Match(fileName);

                if (match1.Success) Console.WriteLine(match1.Groups["aTextInsideBrackets"].Value);

                var match2 = regex2.Match(fileName);

                if (match2.Success) Console.WriteLine(match2.Groups["aTextInsideBrackets"].Value);



            }
        }

        private void button8_Click(object sender, EventArgs e)
        {
            try
            {
                var process = DCRSWorkflowEngine.ProcessRepositoryProvider.LoadProcess(6);

                //var serializedXml = DCRSProcess.Serialize(process);

                //var dcrsProcess = DCRSProcess.Deserialize(serializedXml);

                process.Specification.ProcessId = 12;

                DCRSWorkflowEngine.ProcessRepositoryProvider.SaveProcess(process);

                var processInstance = DCRSWorkflowEngine.ProcessRepositoryProvider.LoadProcessInstance(5, 55);

                var newProcessInstance = new DCRSProcess
                                             {
                                                 Specification = processInstance.Specification,
                                                 Runtime =
                                                     new DCRSRuntime(processInstance.Runtime.CurrentState, 57,
                                                                     processInstance.Runtime.ExecutionTrace)
                                             };

                DCRSWorkflowEngine.ProcessRepositoryProvider.SaveProcessInstance(newProcessInstance);

            }
            catch (Exception exception)
            {

                MessageBox.Show(exception.Message);
            }



        }

        private void button9_Click(object sender, EventArgs e)
        {
            var xmlRepositoryProvider = DCRSWorkflowEngine.ProcessRepositoryProvider;

            var maxProcessInstanceId = (xmlRepositoryProvider.GetProcessInstancesList(555).Count > 0)
                                           ? xmlRepositoryProvider.GetProcessInstancesList(555).Max() + 1
                                           : -1;

            Console.WriteLine(maxProcessInstanceId);
        }

        private void button10_Click(object sender, EventArgs e)
        {
            //DCRSWorkflowEngine.ProcessRepositoryProvider.CloseNotificationHost();
        }

        private void button11_Click(object sender, EventArgs e)
        {

        }
    }
}
