/// Grid based force based algorithm. (this so needs a better name :P)
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;

namespace ITU.DK.DCRS.Visualization.Layout
{
  class GBFBALayoutProvider<ST, LT> : LayoutProvider<ST, LT> where ST : IEquatable<ST>
  {
        private Dictionary<ST, Vector2> NodePositions;
        private Dictionary<ST, Vector2> NodeTensions;
        Graph<ST, LT> TargetGraph;
        private ST[,] Grid;
        private Set<ST> StabileNodes;
        private Set<ST> PausedNodes;

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
            Grid = new ST[20, 20]; // eventually set limits dynamically...
        }

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



        // include a check here to see if they're neighbours and then not add any tension?
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

        public void initRun()
        {
            Random rand = new Random(3);

            NodeTensions.Clear();
            NodePositions.Clear();
            foreach (ST s in TargetGraph.States.Difference(StabileNodes))
            {
              NodeTensions.Add(s, new Vector2(0, 0));
              while (!SetNodePosition(s, new Vector2(rand.Next(7, 13), rand.Next(7, 13))))
                // skip
                ;              
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

        public double changeWithOffset(ST s, Vector2 offset)
        {
          double result = 0;
          Vector2 test = CalcTensionWithOffset(s, offset);
          result = test.Magnitude;

          if ((offset.X == 0) && offset.Y == 0) return result;

          if ((int)(NodePositions[s] + offset).X > 20) return 9999999;
          if ((int)(NodePositions[s] + offset).Y > 20) return 9999999;
          if ((int)(NodePositions[s] + offset).X < 0) return 9999999;
          if ((int)(NodePositions[s] + offset).Y < 0) return 9999999;

          if (!(Grid[(int)(NodePositions[s] + offset).X, (int)(NodePositions[s] + offset).Y]).Equals(default(ST)))
            result += CalcTensionWithOffset(Grid[(int)(NodePositions[s] + offset).X, (int)(NodePositions[s] + offset).Y], offset * -1).Magnitude;         

          return result;
        }

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
