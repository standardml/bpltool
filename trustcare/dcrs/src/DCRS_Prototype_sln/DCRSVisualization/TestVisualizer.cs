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
    /// <summary>
    /// Mainly a class for testing layoutproviders, since it allows for visualizing every step of a layouting algorithm.
    /// </summary>
    public class TestVisualizer
    {
      GBFBALayoutProvider<short, bool> layoutProvider; 
      public void SetupPlacement(DCRS.CommonTypes.Process.DCRSSpecification spec)
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
          g.AddEdge(srcAction, dstAction, true);
        }

        for (var index = 0; index < spec.Responses.GetLength(0); index++)
        {
          short srcAction = spec.Responses[index, 0];
          short dstAction = spec.Responses[index, 1];
          g.AddEdge(srcAction, dstAction, true);
        }


        for (var index = 0; index < spec.Includes.GetLength(0); index++)
        {
          short srcAction = spec.Includes[index, 0];
          short dstAction = spec.Includes[index, 1];
          g.AddEdge(srcAction, dstAction, true);
        }


        for (var index = 0; index < spec.Excludes.GetLength(0); index++)
        {
          short srcAction = spec.Excludes[index, 0];
          short dstAction = spec.Excludes[index, 1];
          g.AddEdge(srcAction, dstAction, true);
        }

        //g.UniqueEdges();

        layoutProvider = new GBFBALayoutProvider<short, bool>(g);
        layoutProvider.initRun();

      }

        public Dictionary<short, Point> CalculatePlacement(DCRS.CommonTypes.Process.DCRSSpecification spec)
        {
            Dictionary<short, Point> result = new Dictionary<short, Point>();
          
          layoutProvider.RunOnce();
            foreach (var x in layoutProvider.GetNodePositions())
            {
              result.Add(x.Key, new Point((int)Math.Round(x.Value.X) * 200, (int)Math.Round(x.Value.Y) * 200));
            }

            int minX = 9999999;
            int minY = 9999999;
            foreach (var x in result)
            {
              minX = Math.Min(x.Value.X, minX);
              minY = Math.Min(x.Value.Y, minY);
            }


            //foreach (var x in result)
            //{
            //              x.Value.X -= minX;
            //x.Value.Y -= minY;
            //}

            Dictionary<short, Point> result2 = new Dictionary<short, Point>();

            foreach (var x in result)
            {
              result2.Add(x.Key, new Point((x.Value.X - minX) + 55, (x.Value.Y - minY) + 85));
            }

            return result2;        

        }

        public void Draw(DCRS.CommonTypes.Process.DCRSSpecification spec, Graphics g)
        {
          /*
          spec.Responses[1, 0] = 1;
          spec.Responses[1, 1] = 1;


          spec.Conditions[1, 0] = 2;
          spec.Conditions[1, 1] = 2;

          spec.Includes[1, 0] = 3;
          spec.Includes[1, 1] = 3;


          spec.Excludes[1, 0] = 0;
          spec.Excludes[1, 1] = 0;
          */


            g.SmoothingMode = SmoothingMode.AntiAlias;

            var placement = CalculatePlacement(spec);

            //Pen acticityPen = Pens.Black;
            //Font activityFont = SystemFonts.DefaultFont;
            //Brush activityBrush = Brushes.Black;

            //Pen arrowPen = Pens.Black;
            //Brush arrowBrush = Brushes.Black;

            Pen acticityPen = new Pen(Color.Black, 2f);
            Font activityFont = new Font(FontFamily.GenericSansSerif, 10f);
            Brush activityBrush = Brushes.Black;

            Pen arrowPen = new Pen(Color.Black, 1f);
            Brush arrowBrush = Brushes.Black;



            Dictionary<short, ActionNode> nodeDict = new Dictionary<short, ActionNode>();

            Set<Arrow> arrows = new Set<Arrow>();
            Set<Arrow> selfarrows = new Set<Arrow>();

            foreach (var p in placement)
            {
              ActionNode n = new ActionNode(p.Key, spec.ActionList[p.Key], new Vector2(p.Value), acticityPen, activityBrush, activityFont);
              n.SetRoles(spec.ActionsToRolesDictionary[p.Key]);
              n.Draw(g);
              nodeDict.Add(p.Key, n);
            }


            for (var index = 0; index < spec.Conditions.GetLength(0); index++)
            {
              short srcAction = spec.Conditions[index, 1];
              short dstAction = spec.Conditions[index, 0];
              ConditionArrow Arrow = new ConditionArrow(arrowBrush, arrowPen, nodeDict[srcAction], nodeDict[dstAction]);
              //Arrow.Draw(g);  
              if (!srcAction.Equals(dstAction))
                arrows.Add(Arrow);
              else
                selfarrows.Add(Arrow);          
            }

                      
            for (var index = 0; index < spec.Responses.GetLength(0); index++)
            {
              short srcAction = spec.Responses[index, 0];
              short dstAction = spec.Responses[index, 1];
              ResponseArrow Arrow = new ResponseArrow(arrowBrush, arrowPen, nodeDict[srcAction], nodeDict[dstAction]);
              //Arrow.Draw(g);
              if (!srcAction.Equals(dstAction))
                arrows.Add(Arrow);
              else
                selfarrows.Add(Arrow);
            }

            for (var index = 0; index < spec.Includes.GetLength(0); index++)
            {
              short srcAction = spec.Includes[index, 0];
              short dstAction = spec.Includes[index, 1];
              InclusionArrow Arrow = new InclusionArrow(arrowBrush, arrowPen, activityFont, nodeDict[srcAction], nodeDict[dstAction]);
              //Arrow.Draw(g);
              if (!srcAction.Equals(dstAction))
                arrows.Add(Arrow);
              else
                selfarrows.Add(Arrow);
            }


            for (var index = 0; index < spec.Excludes.GetLength(0); index++)
            {
              short srcAction = spec.Excludes[index, 0];
              short dstAction = spec.Excludes[index, 1];
              ExclusionArrow Arrow = new ExclusionArrow(arrowBrush, arrowPen, activityFont, nodeDict[srcAction], nodeDict[dstAction]);
              //Arrow.Draw(g);
              if (!srcAction.Equals(dstAction))
                arrows.Add(Arrow);
              else
                selfarrows.Add(Arrow);
            }

            foreach (var a in arrows)
              a.Draw(g);

            foreach (var a in selfarrows)
              a.Draw(g);

        }

    }
}
