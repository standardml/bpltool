﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Drawing.Drawing2D;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.Visualization.Layout;
using ITU.DK.DCRS.Visualization.Elements;

namespace ITU.DK.DCRS.Visualization
{
    public class Visualizer
    {
        private DCRSProcess Process;
        private DCRSSpecification Specification { get { return Process.Specification; } }
        private Dictionary<short, Point> Placement;
        private LayoutProvider<short, bool> LayoutProvider;
        private Size ImageSize;

        private Pen acticityPen = new Pen(Color.Black, 2f);
        private Font activityFont = new Font(FontFamily.GenericSansSerif, 10f);
        private Brush activityBrush = Brushes.Black;

        private Pen arrowPen = new Pen(Color.Black, 1f);
        private Brush arrowBrush = Brushes.Black;


        public Visualizer(DCRSProcess p)
        {
            Process = p;
            LayoutProvider = new StoredLayout<short, bool>(p, CreateGraphFromSpec(p.Specification));
            CalculatePlacement();
            SetUp();
        }


        public void UpdateProcess(DCRSProcess p)
        {
            if (p.Specification.ProcessId == Process.Specification.ProcessId && p.Runtime.ProcessInstanceId == Process.Runtime.ProcessInstanceId)
            {
                Process = p;
                UpdateRuntime();
            }
            else
            {
                Process = p;
                SetUp();
            }
        }

        private void UpdateRuntime()
        {
            /// Build up the nodes
            foreach (var x in Nodes)
            {               
                if (Process.Runtime != null) x.Value.ApplyRuntime(Process.Runtime);                
            }
        }


        /// <summary>
        /// Method for visualizing a dcrs on a bitmap image.
        /// </summary>
        /// <param name="spec"></param>
        /// <returns></returns>
        public Bitmap Visualize()
        {
            Bitmap result = new Bitmap(ImageSize.Width, ImageSize.Height);
            Graphics.FromImage(result).FillRegion(Brushes.White, new Region(new Rectangle(0, 0, ImageSize.Width, ImageSize.Height)));
            Draw(Graphics.FromImage(result));            
            return result;
        }

        /// <summary>
        /// Method for determining the intended placement of nodes in the image to be generated.
        /// </summary>
        /// <param name="spec"></param>
        /// <param name="layoutProvider"></param>
        /// <returns></returns>
        private void CalculatePlacement()
        {
            Placement = new Dictionary<short, Point>();
                       
            LayoutProvider.Run();

            foreach (var x in LayoutProvider.GetNodePositions())
            {
                Placement.Add(x.Key, new Point((int)Math.Round(x.Value.X), (int)Math.Round(x.Value.Y)));
            }

            Placement = ShiftPlacementTowardsTopLeft(Placement);

            CalculateImageSize();
        }

        private void CalculateImageSize()
        {
            int maxX = 0;
            int maxY = 0;

            foreach (var x in Placement)
            {
                maxX = Math.Max(x.Value.X, maxX);
                maxY = Math.Max(x.Value.Y, maxY);
            }

            ImageSize = new Size(maxX + 200, maxY + 200);
        }



        Dictionary<short, ActionNode> Nodes = new Dictionary<short, ActionNode>();

        Set<Arrow> Arrows = new Set<Arrow>();
        Set<Arrow> SelfArrows = new Set<Arrow>();

        private void SetUp()
        {
            /// Build up the nodes
            foreach (var p in Placement)
            {
                ActionNode n = new ActionNode(p.Key, Specification.ActionList[p.Key], new Vector2(p.Value), acticityPen, activityBrush, activityFont);
                if (Process.Runtime != null) n.ApplyRuntime(Process.Runtime);
                n.SetRoles(Specification.ActionsToRolesDictionary[p.Key]);                
                Nodes.Add(p.Key, n);
            }


            ShortHandFinder shf = new ShortHandFinder(Specification);
            
            // soemhow generalize this:
            foreach (var x in shf.Conditions)
                foreach (var y in x.Value)
                {
                    short srcAction = y.Key;
                    short dstAction = x.Key;
                    ConditionArrow Arrow = new ConditionArrow(arrowBrush, arrowPen, Nodes[srcAction], Nodes[dstAction]);
                    if (!srcAction.Equals(dstAction))
                        Arrows.Add(Arrow);
                    else
                        SelfArrows.Add(Arrow);
                }


            foreach (var x in shf.Responses)
                foreach (var y in x.Value)
                {
                    short srcAction = x.Key;
                    short dstAction = y.Key;
                    ResponseArrow Arrow = new ResponseArrow(arrowBrush, arrowPen, Nodes[srcAction], Nodes[dstAction]);
                    if (!srcAction.Equals(dstAction))
                        Arrows.Add(Arrow);
                    else
                        SelfArrows.Add(Arrow);
                }



            foreach (var x in shf.Includes)
                foreach (var y in x.Value)
                {
                    short srcAction = x.Key;
                    short dstAction = y.Key;
                    InclusionArrow Arrow = new InclusionArrow(arrowBrush, arrowPen, activityFont, Nodes[srcAction], Nodes[dstAction]);
                    if (!srcAction.Equals(dstAction))
                        Arrows.Add(Arrow);
                    else
                        SelfArrows.Add(Arrow);
                }


            foreach (var x in shf.Excludes)
                foreach (var y in x.Value)
                {
                    short srcAction = x.Key;
                    short dstAction = y.Key;
                    ExclusionArrow Arrow = new ExclusionArrow(arrowBrush, arrowPen, activityFont, Nodes[srcAction], Nodes[dstAction]);
                    if (!srcAction.Equals(dstAction))
                        Arrows.Add(Arrow);
                    else
                        SelfArrows.Add(Arrow);
                }



            foreach (var x in shf.ConditionResponses)
                foreach (var y in x.Value)
                {
                    short srcAction = x.Key;
                    short dstAction = y.Key;
                    ConditionReponseArrow Arrow = new ConditionReponseArrow(arrowBrush, arrowPen, Nodes[srcAction], Nodes[dstAction]);
                    if (!srcAction.Equals(dstAction))
                        Arrows.Add(Arrow);
                    else
                        SelfArrows.Add(Arrow);
                }


            foreach (var x in shf.Mutexes)
                foreach (var y in x.Value)
                {
                    short srcAction = x.Key;
                    short dstAction = y.Key;
                    MutualExclusionArrow Arrow = new MutualExclusionArrow(arrowBrush, arrowPen, activityFont, Nodes[srcAction], Nodes[dstAction]);
                    if (!srcAction.Equals(dstAction))
                        Arrows.Add(Arrow);
                    else
                        SelfArrows.Add(Arrow);
                }

        }


        /// <summary>
        /// Main method for drawing a specification, that takes a graphics object to draw on.
        /// </summary>
        /// <param name="spec"></param>
        /// <param name="g"></param>
        public void Draw(Graphics g)
        {
            DCRSSpecification spec = Specification;
            g.SmoothingMode = SmoothingMode.AntiAlias;

            /// Draw the nodes first.
            foreach (var x in Nodes)
                x.Value.Draw(g);

            /// Draw normal arrows.
            foreach (var a in Arrows)
                a.Draw(g);

            /// Draw arrows that loop back upon the same node last.
            foreach (var a in SelfArrows)
                a.Draw(g);
        }
        
        
        #region Static Functionality

        /// <summary>
        /// Method for visualizing a dcrs on a bitmap image.
        /// </summary>
        /// <param name="spec"></param>
        /// <returns></returns>
        public static Bitmap Visualize(DCRS.CommonTypes.Process.DCRSProcess proc)
        {

            // set size dynamically?
            Point p = CalculateRequiredSize(proc);

            Bitmap result = new Bitmap(p.X, p.Y);
            Graphics.FromImage(result).FillRegion(Brushes.White, new Region(new Rectangle(0, 0, p.X, p.Y)));
            Draw(proc, Graphics.FromImage(result));
            //Graphics.FromImage(result).DrawString(p.X.ToString() + ":" + p.Y.ToString(), SystemFonts.DefaultFont, Brushes.Aqua, new Point(20, 20));
            return result;
        }


        public static Bitmap VisualizePrincipalView(DCRS.CommonTypes.Process.DCRSProcess proc, string principal)
        {

            // set size dynamically?
            Point p = CalculateRequiredSize(proc);

            Bitmap result = new Bitmap(p.X, p.Y);
            Graphics.FromImage(result).FillRegion(Brushes.White, new Region(new Rectangle(0, 0, p.X, p.Y)));
            DrawPrincipalView(proc, Graphics.FromImage(result), principal);
            //Graphics.FromImage(result).DrawString(p.X.ToString() + ":" + p.Y.ToString(), SystemFonts.DefaultFont, Brushes.Aqua, new Point(20, 20));
            return result;
        }


        public static Point CalculateRequiredSize(DCRS.CommonTypes.Process.DCRSProcess proc)
        {
            //var a = CalculatePlacement(proc.Specification);
            var a = CalculatePlacement(proc.Specification, new StoredLayout<short, bool>(proc, CreateGraphFromSpec(proc.Specification)));
            int maxX = 0;
            int maxY = 0;

            foreach (var x in a)
            {
                maxX = Math.Max(x.Value.X, maxX);
                maxY = Math.Max(x.Value.Y, maxY);
            }

            return new Point(maxX + 200, maxY + 200);
        }


        public static Bitmap Visualize(DCRS.CommonTypes.Process.DCRSSpecification spec)
        {
            DCRS.CommonTypes.Process.DCRSProcess p = new CommonTypes.Process.DCRSProcess();
            p.Specification = spec;
            return Visualize(p);
        }

        /// <summary>
        /// Method for determining the intended placement of nodes in the image to be generated.
        /// </summary>
        /// <param name="spec"></param>
        /// <param name="layoutProvider"></param>
        /// <returns></returns>
        private static Dictionary<short, Point> CalculatePlacement(DCRS.CommonTypes.Process.DCRSSpecification spec, LayoutProvider<short, bool> layoutProvider = null)
        {
            Dictionary<short, Point> result = new Dictionary<short, Point>();

            int c = 0;

            if (layoutProvider == null) layoutProvider = new GBFBALayoutProvider<short, bool>(CreateGraphFromSpec(spec));
            layoutProvider.Run();

            foreach (var x in layoutProvider.GetNodePositions())
            {
                result.Add(x.Key, new Point((int)Math.Round(x.Value.X), (int)Math.Round(x.Value.Y)));
            }

            result = ShiftPlacementTowardsTopLeft(result);

            return result;

        }

        /// <summary>
        /// Helper method to CalculatePlacement that shifts nodes so that diagram is drawn as far as possible in the left-top corner of the image.
        /// </summary>
        /// <param name="result"></param>
        /// <returns></returns>
        private static Dictionary<short, Point> ShiftPlacementTowardsTopLeft(Dictionary<short, Point> result)
        {
            int minX = 9999999;
            int minY = 9999999;
            foreach (var x in result)
            {
                minX = Math.Min(x.Value.X, minX);
                minY = Math.Min(x.Value.Y, minY);
            }

            Dictionary<short, Point> result2 = new Dictionary<short, Point>();

            foreach (var x in result)
            {
                result2.Add(x.Key, new Point((x.Value.X - minX) + 55 + 100, (x.Value.Y - minY) + 85 + 30));
            }
            return result2;
        }

        /// <summary>
        /// Helper function to CalculatePlacement that generates a graph based on a specification. (Because the layoutProviders are generalized to work on graphs).
        /// </summary>
        /// <param name="spec"></param>
        /// <returns></returns>
        internal static Graph<short, bool> CreateGraphFromSpec(DCRS.CommonTypes.Process.DCRSSpecification spec)
        {
            Graph<short, bool> g = new Graph<short, bool>();
            foreach (var a in spec.ActionList)
            {
                g.AddState(a.Key);
            }

            for (var index = 0; index < spec.Conditions.GetLength(0); index++)
            {
                short srcAction = spec.Conditions[index, 1];
                short dstAction = spec.Conditions[index, 0];
                if (srcAction != dstAction) g.AddEdge(srcAction, dstAction, true);
            }

            for (var index = 0; index < spec.Responses.GetLength(0); index++)
            {
                short srcAction = spec.Responses[index, 0];
                short dstAction = spec.Responses[index, 1];
                if (srcAction != dstAction) g.AddEdge(srcAction, dstAction, true);
            }


            for (var index = 0; index < spec.Includes.GetLength(0); index++)
            {
                short srcAction = spec.Includes[index, 0];
                short dstAction = spec.Includes[index, 1];
                if (srcAction != dstAction) g.AddEdge(srcAction, dstAction, true);
            }


            for (var index = 0; index < spec.Excludes.GetLength(0); index++)
            {
                short srcAction = spec.Excludes[index, 0];
                short dstAction = spec.Excludes[index, 1];
                if (srcAction != dstAction) g.AddEdge(srcAction, dstAction, true);
            }
            return g;
        }


        public static void Draw(DCRS.CommonTypes.Process.DCRSSpecification spec, Graphics g)
        {
            DCRS.CommonTypes.Process.DCRSProcess p = new CommonTypes.Process.DCRSProcess();
            p.Specification = spec;
            Draw(p, g);
        }

        /// <summary>
        /// Main method fro drawing a specification, that takes a graphics object to draw on.
        /// </summary>
        /// <param name="spec"></param>
        /// <param name="g"></param>
        public static void Draw(DCRS.CommonTypes.Process.DCRSProcess proc, Graphics g)
        {
            DCRS.CommonTypes.Process.DCRSSpecification spec = proc.Specification;
            g.SmoothingMode = SmoothingMode.AntiAlias;

            //var placement = CalculatePlacement(spec);
            var placement = CalculatePlacement(spec, new StoredLayout<short, bool>(proc, CreateGraphFromSpec(spec)));

            // !! Make these public fields at some point so that they can be set programatically. !!
            Pen acticityPen = new Pen(Color.Black, 2f);
            Font activityFont = new Font(FontFamily.GenericSansSerif, 10f);
            Brush activityBrush = Brushes.Black;

            Pen arrowPen = new Pen(Color.Black, 1f);
            Brush arrowBrush = Brushes.Black;

            Dictionary<short, ActionNode> nodeDict = new Dictionary<short, ActionNode>();

            Set<Arrow> arrows = new Set<Arrow>();
            Set<Arrow> selfarrows = new Set<Arrow>();

            /// Draw the nodes first.
            foreach (var p in placement)
            {
                ActionNode n = new ActionNode(p.Key, spec.ActionList[p.Key], new Vector2(p.Value), acticityPen, activityBrush, activityFont);
                if (proc.Runtime != null) n.ApplyRuntime(proc.Runtime);
                n.SetRoles(spec.ActionsToRolesDictionary[p.Key]);
                n.Draw(g);
                nodeDict.Add(p.Key, n);
            }

            ShortHandFinder shf = new ShortHandFinder(spec);

            foreach (var x in shf.Conditions)
                foreach (var y in x.Value)
                {
                    short srcAction = y.Key;
                    short dstAction = x.Key;
                    ConditionArrow Arrow = new ConditionArrow(arrowBrush, arrowPen, nodeDict[srcAction], nodeDict[dstAction]);
                    if (!srcAction.Equals(dstAction))
                        arrows.Add(Arrow);
                    else
                        selfarrows.Add(Arrow);
                }


            foreach (var x in shf.Responses)
                foreach (var y in x.Value)
                {
                    short srcAction = x.Key;
                    short dstAction = y.Key;
                    ResponseArrow Arrow = new ResponseArrow(arrowBrush, arrowPen, nodeDict[srcAction], nodeDict[dstAction]);
                    if (!srcAction.Equals(dstAction))
                        arrows.Add(Arrow);
                    else
                        selfarrows.Add(Arrow);
                }



            foreach (var x in shf.Includes)
                foreach (var y in x.Value)
                {
                    short srcAction = x.Key;
                    short dstAction = y.Key;
                    InclusionArrow Arrow = new InclusionArrow(arrowBrush, arrowPen, activityFont, nodeDict[srcAction], nodeDict[dstAction]);
                    if (!srcAction.Equals(dstAction))
                        arrows.Add(Arrow);
                    else
                        selfarrows.Add(Arrow);
                }


            foreach (var x in shf.Excludes)
                foreach (var y in x.Value)
                {
                    short srcAction = x.Key;
                    short dstAction = y.Key;
                    ExclusionArrow Arrow = new ExclusionArrow(arrowBrush, arrowPen, activityFont, nodeDict[srcAction], nodeDict[dstAction]);
                    if (!srcAction.Equals(dstAction))
                        arrows.Add(Arrow);
                    else
                        selfarrows.Add(Arrow);
                }



            foreach (var x in shf.ConditionResponses)
                foreach (var y in x.Value)
                {
                    short srcAction = x.Key;
                    short dstAction = y.Key;
                    ConditionReponseArrow Arrow = new ConditionReponseArrow(arrowBrush, arrowPen, nodeDict[srcAction], nodeDict[dstAction]);
                    if (!srcAction.Equals(dstAction))
                        arrows.Add(Arrow);
                    else
                        selfarrows.Add(Arrow);
                }


            foreach (var x in shf.Mutexes)
                foreach (var y in x.Value)
                {
                    short srcAction = x.Key;
                    short dstAction = y.Key;
                    MutualExclusionArrow Arrow = new MutualExclusionArrow(arrowBrush, arrowPen, activityFont, nodeDict[srcAction], nodeDict[dstAction]);
                    if (!srcAction.Equals(dstAction))
                        arrows.Add(Arrow);
                    else
                        selfarrows.Add(Arrow);
                }

            /// Draw normal arrows.
            foreach (var a in arrows)
                a.Draw(g);

            /// Draw arrows that loop back upon the same node last.
            foreach (var a in selfarrows)
                a.Draw(g);
        }

        public static void DrawPrincipalView(DCRS.CommonTypes.Process.DCRSProcess proc, Graphics g, String principal)
        {
            DCRS.CommonTypes.Process.DCRSSpecification spec = proc.Specification;


            List<String> roles = new List<string>();

            foreach (var x in spec.RolesToPrincipalsDictionary)
            {
                if (x.Value.Contains(principal)) roles.Add(x.Key);
            }

            g.SmoothingMode = SmoothingMode.AntiAlias;

            //var placement = CalculatePlacement(spec);
            var placement = CalculatePlacement(spec, new StoredLayout<short, bool>(proc, CreateGraphFromSpec(spec)));

            // !! Make these public fields at some point so that they can be set programatically. !!
            Pen acticityPen = new Pen(Color.Black, 2f);
            Font activityFont = new Font(FontFamily.GenericSansSerif, 10f);
            Brush activityBrush = Brushes.Black;

            Pen arrowPen = new Pen(Color.Black, 1f);
            Brush arrowBrush = Brushes.Black;

            Dictionary<short, ActionNode> nodeDict = new Dictionary<short, ActionNode>();

            Set<Arrow> arrows = new Set<Arrow>();
            Set<Arrow> selfarrows = new Set<Arrow>();

            /// Draw the nodes first.
            foreach (var p in placement)
            {
                if (spec.ActionsToRolesDictionary[p.Key].Intersect(roles).Count() > 0)
                {
                    ActionNode n = new ActionNode(p.Key, spec.ActionList[p.Key], new Vector2(p.Value), acticityPen, activityBrush, activityFont);
                    if (proc.Runtime != null) n.ApplyRuntime(proc.Runtime);
                    n.SetRoles(spec.ActionsToRolesDictionary[p.Key]);
                    n.Draw(g);
                    nodeDict.Add(p.Key, n);
                }
            }
        }


        // we wont need this when we unstatic the class.
        public static short GetActionByPos(Point p, DCRSProcess proc)
        {
            var a = CalculatePlacement(proc.Specification, new StoredLayout<short, bool>(proc, CreateGraphFromSpec(proc.Specification)));

            foreach (var x in a)
            {
                if (p.X < x.Value.X + 50 && p.X > x.Value.X - 50)
                    if (p.Y < x.Value.Y + 50 && p.Y > x.Value.Y - 50)
                        return x.Key;
            }
            return -1;
        } 
        #endregion

    }
}
