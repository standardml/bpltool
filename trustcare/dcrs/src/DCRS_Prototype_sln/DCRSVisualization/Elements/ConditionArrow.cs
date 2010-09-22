﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Drawing.Drawing2D;

namespace ITU.DK.DCRS.Visualization.Elements
{
  public class ConditionArrow : Arrow
  {

    public ConditionArrow(System.Drawing.Brush arrowBrush, System.Drawing.Pen arrowPen, ActionNode actionNode, ActionNode actionNode_2) 
      : base(arrowBrush, arrowPen, actionNode, actionNode_2)
    {
    }

    public override void Draw(Graphics g)
    {
        arrowBrush = Brushes.Blue;
        arrowPen.Brush = arrowBrush;
      base.Draw(g);
      g.FillEllipse(arrowBrush, ArrowDst.ToPoint.X - 3, ArrowDst.ToPoint.Y - 3, 6, 6);
    }
  }
}
