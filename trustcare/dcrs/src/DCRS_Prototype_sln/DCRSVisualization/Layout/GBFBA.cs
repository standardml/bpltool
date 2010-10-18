using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;

namespace ITU.DK.DCRS.Visualization.Layout
{
    /// <summary>
    /// Layoutprovider that implements a force-based inspired algorithm that keeps states in a strict grid,
    /// does not allow them to overlap and under those conditions moves them closer together based on tension
    /// provided by edges.
    /// </summary>
    /// <typeparam name="ST">State type</typeparam>
    /// <typeparam name="LT">Label type</typeparam>
  class GBFBALayoutProvider<ST, LT> : LayoutProvider<ST, LT> where ST : IEquatable<ST>
  {
        private Dictionary<ST, Vector2> NodePositions;
        private Dictionary<ST, Vector2> NodeTensions;
        Graph<ST, LT> TargetGraph;
        private ST[,] Grid;
        private Set<ST> StabileNodes;
        private Set<ST> PausedNodes;
        protected int GridWidth = 100;
        protected int GridHeight = 100;

        public Dictionary<ST, Vector2> GetNodePositions()
        {
            Dictionary<ST, Vector2> result = new Dictionary<ST, Vector2>();
            foreach (var a in NodePositions)
                result.Add(a.Key, a.Value * 200);
            return result;
        }

        public GBFBALayoutProvider(Graph<ST, LT> g)
        {
            NodeTensions = new Dictionary<ST, Vector2>();
            NodePositions = new Dictionary<ST, Vector2>();
            StabileNodes = new Set<ST>();
            PausedNodes = new Set<ST>();
            TargetGraph = g;
            Grid = new ST[GridWidth, GridHeight]; // eventually set limits dynamically...
        }

        /// <summary>
        /// Sets the position of a node s to a position p, if that position is empty.
        /// </summary>
        /// <param name="s"></param>
        /// <param name="p"></param>
        /// <returns>False if a node already exists in position p.</returns>
        public bool SetNodePosition(ST s, Vector2 p)
        {
          if (Grid[(int)p.X, (int)p.Y].Equals(default(ST)))
          {
            Grid[(int)p.X, (int)p.Y] = s;
            NodePositions.Add(s, p);
            return true;
          }
          else
            return false;
        }
        
        /// <summary>
        /// Changes the position of a node s to a position p. If there is already a node s2 in p, then s2 will be moved to the current position of s.
        /// </summary>
        /// <param name="s"></param>
        /// <param name="p"></param>
        public void ChangeNodePosition(ST s, Vector2 p)
        {
          if (Grid[(int)p.X, (int)p.Y] == null)
          {
            Grid[(int)p.X, (int)p.Y] = s;
            NodePositions[s] = p;
          }
          else
          {
            NodePositions[Grid[(int)p.X, (int)p.Y]] = NodePositions[s];
            Grid[(int)NodePositions[s].X, (int)NodePositions[s].Y] = Grid[(int)p.X, (int)p.Y];

            Grid[(int)p.X, (int)p.Y] = s;
            NodePositions[s] = p;
          }
        }
  
        /// <summary>
        /// Calculates the tension on a node s.
        /// </summary>
        /// <param name="s"></param>
        /// <returns></returns>
        public Vector2 CalcTension(ST s)
        {
            Vector2 result = new Vector2(0, 0);
            foreach (Edge<ST, LT> e in TargetGraph.OutgoingEdges[s])
            {
            result += NodePositions[e.d] - NodePositions[s];            
            }

            foreach (Edge<ST, LT> e in TargetGraph.IncomingEdges[s])
            {
            result += NodePositions[e.s] - NodePositions[s];            
            }
            return result;
        }

        /// <summary>
        /// Calculates hypothetical tension on a node s, in case some offset is applied to it.
        /// </summary>
        /// <param name="s"></param>
        /// <param name="offset"></param>
        /// <returns></returns>
        public Vector2 CalcTensionWithOffset(ST s, Vector2 offset)
        {
          Vector2 result = new Vector2(0, 0);
          foreach (Edge<ST, LT> e in TargetGraph.OutgoingEdges[s])
          {
            if (((NodePositions[e.d] - (NodePositions[s] + offset)).X == 0) && ( (NodePositions[e.d] - (NodePositions[s] + offset)).Y == 0))
              result += (NodePositions[e.d] - offset) - (NodePositions[s] + offset);
            else
              result += NodePositions[e.d] - (NodePositions[s] + offset);
            //result += NodePositions[e.d] - (NodePositions[s] + offset);
          }

          foreach (Edge<ST, LT> e in TargetGraph.IncomingEdges[s])
          {
            //result += NodePositions[e.s] - (NodePositions[s] + offset);
            if (((NodePositions[e.s] - (NodePositions[s] + offset)).X == 0) && ((NodePositions[e.s] - (NodePositions[s] + offset)).Y == 0))
              result += (NodePositions[e.s] - offset) - (NodePositions[s] + offset);
            else
              result += NodePositions[e.s] - (NodePositions[s] + offset);

          }
          return result;
        }


        /// <summary>
        /// Finds the node with the highest tension on it.
        /// </summary>
        /// <returns></returns>
        public ST MaxTensionNode()
        {
          double maxTension = 0;
          ST result = default(ST);
          foreach (var x in NodeTensions)
          {
            if ((x.Value.Magnitude > maxTension) && (!PausedNodes.Contains(x.Key))) 
            {
              maxTension = x.Value.Magnitude;
              result = x.Key;
            }
          }
          return result;
        }

        /// <summary>
        /// Initializes the layout provider.
        /// </summary>
        public void initRun()
        {
            Random rand = new Random(/*3*/);

            NodeTensions.Clear();
            NodePositions.Clear();
            foreach (ST s in TargetGraph.States.Difference(StabileNodes))
            {
                // Original method:                
                  NodeTensions.Add(s, new Vector2(0, 0));
                  while (!SetNodePosition(s, new Vector2(rand.Next(7, 13), rand.Next(7, 13))))
                    // skip
                    ;              
                 
                // New method:
                /*
                NodeTensions.Add(s, new Vector2(0, 0));
                int inc = TargetGraph.IncomingEdges[s].Count;
                int outg = TargetGraph.OutgoingEdges[s].Count;
                while (!SetNodePosition(s, new Vector2((((1+inc) * 10) + rand.Next(0, 4)), (((1+inc) * 10) + rand.Next(0, 4)))))
                    // skip
                    ;              
                */

            }

            foreach (ST s in StabileNodes)
            {
                NodeTensions.Add(s, new Vector2(0, 0));
                NodePositions.Add(s, new Vector2(1, 1));
            }

            foreach (ST s in TargetGraph.States.Difference(StabileNodes))
            {
              NodeTensions[s] = CalcTension(s);
            }
        }

        /// <summary>
        ///  Basically calculates the the change in tension on the system in case a node if moved by a certain offset.
        ///  However, it doesn't substract the original tension of the one or two nodes that got moved.
        ///  [[Basically this piece of code should be restructured a bit so that it's easier to understand and document.]]
        /// </summary>
        /// <param name="s"></param>
        /// <param name="offset"></param>
        /// <returns></returns>
        public double changeWithOffset(ST s, Vector2 offset)
        {
          double result = 0;
          Vector2 test = CalcTensionWithOffset(s, offset);
          result = test.Magnitude;

          if ((offset.X == 0) && offset.Y == 0) return result;

          if ((int)(NodePositions[s] + offset).X > GridWidth) return 9999999;
          if ((int)(NodePositions[s] + offset).Y > GridHeight) return 9999999;
          if ((int)(NodePositions[s] + offset).X < 0) return 9999999;
          if ((int)(NodePositions[s] + offset).Y < 0) return 9999999;

          if (!(Grid[(int)(NodePositions[s] + offset).X, (int)(NodePositions[s] + offset).Y]).Equals(default(ST)))
            result += CalcTensionWithOffset(Grid[(int)(NodePositions[s] + offset).X, (int)(NodePositions[s] + offset).Y], offset * -1).Magnitude;         

          return result;
        }

        /// <summary>
        /// Executes the algorithm for a single step.
        /// </summary>
        /// <returns></returns>
        public bool RunOnce()
        {
          ST s = MaxTensionNode();
          double nochangeTen = changeWithOffset(s, new Vector2(0, 0));
          double min = changeWithOffset(s, new Vector2(0, 0));
          double minAction = 0;
          double leftTen = changeWithOffset(s, new Vector2(-1, 0));
          if (leftTen < min)
          {
            min = leftTen;
            minAction = 1;
          }
          double rightTen = changeWithOffset(s, new Vector2(1, 0));
          if (rightTen < min)
          {
            min = rightTen;
            minAction = 2;
          }
          double topTen = changeWithOffset(s, new Vector2(0, -1));
          if (topTen < min)
          {
            min = topTen;
            minAction = 3;
          }
          double bottomTen = changeWithOffset(s, new Vector2(0, 1));
          if (bottomTen < min)
          {
            min = bottomTen;
            minAction = 4;
          }         

          double rightTopTen = changeWithOffset(s, new Vector2(1, -1));          
          if (rightTopTen < min)
          {
            min = rightTopTen;
            minAction = 5;
          }
          double rightBottomTen = changeWithOffset(s, new Vector2(1, 1));
          if (rightBottomTen < min)
          {
            min = rightBottomTen;
            minAction = 6;
          }
          double leftTopTen = changeWithOffset(s, new Vector2(-1, -1));
          if (leftTopTen < min)
          {
            min = leftTopTen;
            minAction = 7;
          }
          double leftBottomTen = changeWithOffset(s, new Vector2(-1, 1));
          if (leftBottomTen < min)
          {
            min = leftBottomTen;
            minAction = 8;
          }

          if (minAction == 0)
          {
            PausedNodes.Add(s);
            return false;
          }
          else if (minAction == 1)
          {
            ChangeNodePosition(s, NodePositions[s] + new Vector2(-1, 0));
          }
          else if (minAction == 2)
          {
            ChangeNodePosition(s, NodePositions[s] + new Vector2(1, 0));
          }
          else if (minAction == 3)
          {
            ChangeNodePosition(s, NodePositions[s] + new Vector2(0, -1));
          }
          else if (minAction == 4)
          {
            ChangeNodePosition(s, NodePositions[s] + new Vector2(0, 1));
          }

          else if (minAction == 5)
          {
            ChangeNodePosition(s, NodePositions[s] + new Vector2(1, -1));
          }
          else if (minAction == 6)
          {
            ChangeNodePosition(s, NodePositions[s] + new Vector2(1, 1));
          }
          else if (minAction == 7)
          {
            ChangeNodePosition(s, NodePositions[s] + new Vector2(-1, -1));
          }
          else if (minAction == 8)
          {
            ChangeNodePosition(s, NodePositions[s] + new Vector2(-1, 1));
          }

          PausedNodes.Clear();
          return true;
        }

        /// <summary>
        /// Executes the algorithm until it ends.
        /// </summary>
        public void Run()
        {
            initRun();
            //while (RunOnce())
//              //skip
              //;
            while (NodeTensions.Count > PausedNodes.Count)
              RunOnce();
        }

  } 
    
}
