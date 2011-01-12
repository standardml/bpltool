using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using DCRStoFiniteAutomaton;
using Microsoft.Glee.Drawing;
namespace WindowsApplication
{
  public partial class Form1 : Form
  {
    ToolTip toolTip1 = new ToolTip();
    public Form1()
    {
      this.Load += new EventHandler(Form1_Load);
      InitializeComponent();

      this.toolTip1.Active = true;
      toolTip1.AutoPopDelay = 5000;
      toolTip1.InitialDelay = 1000;
      toolTip1.ReshowDelay = 500;
      // Force the ToolTip text to       toolTip1.ShowAlways = true;


    }

    void Form1_Load(object sender, EventArgs e)
    {
    //  gViewer.RemoveToolbar();
      gViewer.SelectionChanged += 
        new EventHandler(gViewer_SelectionChanged);

      gViewer.MouseClick += new MouseEventHandler(gViewer_MouseClick);
    }

    void gViewer_MouseClick(object sender, MouseEventArgs e)
    {
      if (e.Button == System.Windows.Forms.MouseButtons.Right)
        MessageBox.Show("right button");
    }

   
    object selectedObjectAttr;
    object selectedObject;
    void gViewer_SelectionChanged(object sender, EventArgs e)
    {

      if (selectedObject != null)
      {
        if (selectedObject is Edge)
          (selectedObject as Edge).Attr = selectedObjectAttr as EdgeAttr;
        else if (selectedObject is Node)
          (selectedObject as Node).Attr = selectedObjectAttr as NodeAttr;

        selectedObject = null;
      }

      if (gViewer.SelectedObject == null)
      {
        label1.Text = "No object under the mouse";
        this.gViewer.SetToolTip(toolTip1, "");

      }
      else
      {
        selectedObject = gViewer.SelectedObject;
      
        if (selectedObject is Edge)
        {
          selectedObjectAttr = (gViewer.SelectedObject as Edge).Attr.Clone();
          (gViewer.SelectedObject as Edge).Attr.Color = Microsoft.Glee.Drawing.Color.Magenta;
          (gViewer.SelectedObject as Edge).Attr.Fontcolor = Microsoft.Glee.Drawing.Color.Magenta;
          Edge edge=gViewer.SelectedObject as Edge;
       
         


        //here you can use e.Attr.Id or e.UserData to get back to you data
          this.gViewer.SetToolTip(this.toolTip1, String.Format("edge from {0} {1}", edge.Source, edge.Target));
     
        }
        else if (selectedObject is Node)
        {

          selectedObjectAttr = (gViewer.SelectedObject as Node).Attr.Clone();
          (selectedObject as Node).Attr.Color = Microsoft.Glee.Drawing.Color.Magenta;
          (selectedObject as Node).Attr.Fontcolor = Microsoft.Glee.Drawing.Color.Magenta;
          //here you can use e.Attr.Id to get back to your data
          this.gViewer.SetToolTip(toolTip1,String.Format("node {0}", (selectedObject as Node).Attr.Label));
        }
        label1.Text = selectedObject.ToString();
      }
      gViewer.Invalidate();
    }

   
 

    private void button1_Click(object sender, EventArgs e)
    {

        DrawExample();
        
    //    //this is abstract.dot of GraphViz
    //  Graph g = new Graph("graph");
    //  g.GraphAttr.NodeAttr.Padding = 3;
    //  Edge edge = (Edge)g.AddEdge("S24", "27");
    //  edge.Attr.Label = "Edge Label Test";
     
    //  g.AddEdge("S24", "25");
    //  edge=g.AddEdge("S1", "10") as Edge;
      
    //  edge.Attr.Label = "Init";
    //  edge.Attr.ArrowHeadAtTarget = ArrowStyle.Tee;
    ////  edge.Attr.Weight = 10;
    //  edge = g.AddEdge("S1", "2") as Edge;
    // // edge.Attr.Weight = 10;
    //  g.AddEdge("S35", "36");
    //  g.AddEdge("S35", "43");
    //  g.AddEdge("S30", "31");
    //  g.AddEdge("S30", "33");
    //  g.AddEdge("9", "42");
    //  g.AddEdge("9", "T1");
    //  g.AddEdge("25", "T1");
    //  g.AddEdge("25", "26");
    //  g.AddEdge("27", "T24");
    //  g.AddEdge("2", "3");
    //  g.AddEdge("2", "16");
    //  g.AddEdge("2", "17");
    //  g.AddEdge("2", "T1");
    //  g.AddEdge("2", "18");
    //  g.AddEdge("10", "11");
    //  g.AddEdge("10", "14");
    //  g.AddEdge("10", "T1");
    //  g.AddEdge("10", "13");
    //  g.AddEdge("10", "12");
    //  g.AddEdge("31", "T1");
    //  edge=(Edge)g.AddEdge("31", "32");
    //  edge.Attr.ArrowHeadAtTarget = ArrowStyle.Tee;
    //  edge.Attr.LineWidth = 10;

    //  edge=(Edge)g.AddEdge("33", "T30");
    //  edge.Attr.LineWidth = 3;
    //  edge.Attr.AddStyle(Microsoft.Glee.Drawing.Style.Dashed);
    //  g.AddEdge("33", "34");
    //  g.AddEdge("42", "4");
    //  g.AddEdge("26", "4");
    //  g.AddEdge("3", "4");
    //  g.AddEdge("16", "15");
    //  g.AddEdge("17", "19");
    //  g.AddEdge("18", "29");
    //  g.AddEdge("11", "4");
    //  g.AddEdge("14", "15");
    //  g.AddEdge("37", "39");
    //  g.AddEdge("37", "41");
    //  g.AddEdge("37", "38");
    //  g.AddEdge("37", "40");
    //  g.AddEdge("13", "19");
    //  g.AddEdge("12", "29");
    //  g.AddEdge("43", "38");
    //  g.AddEdge("43", "40");
    //  g.AddEdge("36", "19");
    //  g.AddEdge("32", "23");
    //  g.AddEdge("34", "29");
    //  g.AddEdge("39", "15");
    //  g.AddEdge("41", "29");
    //  g.AddEdge("38", "4");
    //  g.AddEdge("40", "19");
    //  g.AddEdge("4", "5");
    //  g.AddEdge("19", "21");
    //  g.AddEdge("19", "20");
    //  g.AddEdge("19", "28");
    //  g.AddEdge("5", "6");
    //  g.AddEdge("5", "T35");
    //  g.AddEdge("5", "23");
    //  g.AddEdge("21", "22");
    //  g.AddEdge("20", "15");
    //  g.AddEdge("28", "29");
    //  g.AddEdge("6", "7");
    //  g.AddEdge("15", "T1");
    //  g.AddEdge("22", "23");
    //  g.AddEdge("22", "T35");
    //  g.AddEdge("29", "T30");
    //  g.AddEdge("7", "T8");
    //  g.AddEdge("23", "T24");
    //  g.AddEdge("23", "T1");


    //  Node node = g.FindNode("S1") as Node;
    //  node.Attr.Label = "Label Test";
    //  CreateSourceNode(g.FindNode("S1")as Node);
    //  CreateSourceNode(g.FindNode("S24")as Node);
    //  CreateSourceNode(g.FindNode("S35")as Node);
      
      
    //  CreateTargetNode(g.FindNode("T24") as Node);
    //  CreateTargetNode( g.FindNode("T1")as Node);
    //  CreateTargetNode( g.FindNode("T30")as Node);
    //  CreateTargetNode( g.FindNode("T8")as Node);
      
    //  //layout the graph and draw it
    //  gViewer.Graph = g;
    //  this.propertyGrid1.SelectedObject = g;
    }


      private void DrawExample()
      {

          StateManager stateManager = StateManager.GetStateManagerInstance();

          //stateManager.Specification = DCRSExamples.GetTwoSelfResponseEventsExample();
          stateManager.Specification = DCRSExamples.GetGiveMedicineFullExample();

          Dictionary<long, AtomicState> stateSpace = stateManager.ComputeStateSpace();


          string path = string.Format(@"../../../Documents/{0}.csv", stateManager.Specification.ModelName);

          StreamWriter writer = new StreamWriter(path);


          foreach (var keyvalPair in stateSpace)
          {
              writer.WriteLine(keyvalPair.Value);

          }

          writer.Flush();

          writer.Close();




          Graph graph = new Graph("graph")
                            {
                                BuildNodeHierarchy = true
                                
                            };

          //S


          AddStatesOfRankToGraph(stateManager, graph, 2);

          AddStatesOfRankToGraph(stateManager, graph, 1);

          AddStatesOfRankToGraph(stateManager, graph, 0);


          graph.GraphAttr.NodeAttr.Padding = 3;
          //Edge edge = (Edge)g.AddEdge("S24", "27");
          //edge.Attr.Label = "Edge Label Test";

          gViewer.Graph = graph;

          //gViewer.


      }


      private static void AddStatesOfRankToGraph(StateManager stateManager, Graph graph, short stateRank)
      {

          foreach (var keyValPair in stateManager.ComputedStates)
          {
              if (keyValPair.Value.StateVector.StateRank != stateRank) continue;



              string nodeId = string.Format("s{0}", keyValPair.Value.StateNumber);

              var node = graph.AddNode(nodeId);

              switch (stateRank)
              {
                  case 0:
                      node.Attr.Fillcolor = Color.Lavender;
                      break;
                  case 1:
                      node.Attr.Fillcolor = Color.PeachPuff;
                      break;

                  case 2:
                      node.Attr.Fillcolor = Color.PaleGreen;
                      break;
              }

              if (keyValPair.Value.StateVector.StateAccepting) node.Attr.Shape = Shape.DoubleCircle;
              else node.Attr.Shape = Shape.Msquare;
              

              foreach (Transition transition in keyValPair.Value.Transitions)
              {
                  if (transition.Direction == TransitionDirection.Incoming)
                  {
                      //graph.AddEdge(transition.StateNumber.ToString(),
                      //              keyValPair.Value.LeadingTransition.ToString(),
                      //              keyValPair.Value.StateNumber.ToString());
                  }
                  else
                  {
                      var edgelabel = stateManager.Specification.ActionList[transition.Label];

                      var targetNodeId = string.Format("s{0}", transition.StateNumber);



                      //node.AddOutEdge();
                      var edge = graph.AddEdge(nodeId, edgelabel, targetNodeId);

                      node.AddOutEdge(edge);



                      //graph.AddEdge(nodeId,
                      //             edgelabel,
                      //              transition.StateNumber.ToString());

                  }
              }

          }



      }



    private static void CreateSourceNode(Node a)
    {
      a.Attr.Shape = Microsoft.Glee.Drawing.Shape.Box;
      a.Attr.XRad = 3;
      a.Attr.YRad = 3;
      a.Attr.Fillcolor = Microsoft.Glee.Drawing.Color. Green;
      a.Attr.LineWidth = 10;
    }

    private void CreateTargetNode(Node a)
    {
      a.Attr.Shape = Microsoft.Glee.Drawing.Shape.DoubleCircle;
      a.Attr.Fillcolor = Microsoft.Glee.Drawing.Color.LightGray;

      a.Attr.LabelMargin=-4;
    }


    private void recalculateLayoutButton_Click(object sender, EventArgs e)
    {
      this.gViewer.Graph = this.propertyGrid1.SelectedObject as Microsoft.Glee.Drawing.Graph;

    }

    private void button2_Click(object sender, EventArgs e)
    {
        Graph g = new Graph("graph");
        //g.GraphAttr.NodeAttr.Padding = 3;
        Edge edge = (Edge)g.AddEdge("S24", "27");
        edge.Attr.Label = "Edge Label Test";
        
        Style style = new Style();
        //edge.Attr.AddStyle();

        gViewer.Graph = g;

    }
   
  }
}
