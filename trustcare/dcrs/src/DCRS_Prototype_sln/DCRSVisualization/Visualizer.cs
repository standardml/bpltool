using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Drawing.Drawing2D;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.Visualization.Layout;
using ITU.DK.DCRS.Visualization.Elements;

namespace ITU.DK.DCRS.Visualization
{
    public static class Visualizer
    {

        /// <summary>
        /// Method for visualizing a dcrs on a bitmap image.
        /// </summary>
        /// <param name="spec"></param>
        /// <returns></returns>
        public static Bitmap Visualize(DCRS.CommonTypes.Process.DCRSProcess proc)
        {

            // set size dynamically?
            Bitmap result = new Bitmap(1000,1000);
            Graphics.FromImage(result).FillRegion(Brushes.White, new Region(new Rectangle(0,0,1000,1000)));
            Draw(proc, Graphics.FromImage(result));
            return result;
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

            Graph<short, bool> g = CreateGraphFromSpec(spec);

            if (layoutProvider == null) layoutProvider = new GBFBALayoutProvider<short, bool>(g);
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
        private static Graph<short, bool> CreateGraphFromSpec(DCRS.CommonTypes.Process.DCRSSpecification spec)
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

            var placement = CalculatePlacement(spec);

            //Pen acticityPen = Pens.Black;
            //Font activityFont = SystemFonts.DefaultFont;
            //Brush activityBrush = Brushes.Black;

            //Pen arrowPen = Pens.Black;
            //Brush arrowBrush = Brushes.Black;

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

            /// Add conditions to the lists of arrows to draw.
            for (var index = 0; index < spec.Conditions.GetLength(0); index++)
            {
              short srcAction = spec.Conditions[index, 1];
              short dstAction = spec.Conditions[index, 0];
              ConditionArrow Arrow = new ConditionArrow(arrowBrush, arrowPen, nodeDict[srcAction], nodeDict[dstAction]);
              if (!srcAction.Equals(dstAction))
                arrows.Add(Arrow);
              else
                selfarrows.Add(Arrow);          
            }

            /// Add responses to the lists of arrows to draw.                      
            for (var index = 0; index < spec.Responses.GetLength(0); index++)
            {
              short srcAction = spec.Responses[index, 0];
              short dstAction = spec.Responses[index, 1];
              ResponseArrow Arrow = new ResponseArrow(arrowBrush, arrowPen, nodeDict[srcAction], nodeDict[dstAction]);
              if (!srcAction.Equals(dstAction))
                arrows.Add(Arrow);
              else
                selfarrows.Add(Arrow);
            }

            /// Add includes to the lists of arrows to draw.                      
            for (var index = 0; index < spec.Includes.GetLength(0); index++)
            {
              short srcAction = spec.Includes[index, 0];
              short dstAction = spec.Includes[index, 1];
              InclusionArrow Arrow = new InclusionArrow(arrowBrush, arrowPen, activityFont, nodeDict[srcAction], nodeDict[dstAction]);
              if (!srcAction.Equals(dstAction))
                arrows.Add(Arrow);
              else
                selfarrows.Add(Arrow);
            }

            /// Add excludes to the lists of arrows to draw.                      
            for (var index = 0; index < spec.Excludes.GetLength(0); index++)
            {
              short srcAction = spec.Excludes[index, 0];
              short dstAction = spec.Excludes[index, 1];
              ExclusionArrow Arrow = new ExclusionArrow(arrowBrush, arrowPen, activityFont, nodeDict[srcAction], nodeDict[dstAction]);
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
    }
}
