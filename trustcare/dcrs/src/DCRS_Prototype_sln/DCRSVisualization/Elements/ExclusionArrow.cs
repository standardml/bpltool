using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Drawing.Drawing2D;

namespace ITU.DK.DCRS.Visualization.Elements
{
  public class ExclusionArrow : Arrow
  {
    private System.Drawing.Font arrowFont;

    public ExclusionArrow(System.Drawing.Brush arrowBrush, System.Drawing.Pen arrowPen, System.Drawing.Font arrowFont, ActionNode actionNode, ActionNode actionNode_2)
      : base(arrowBrush, arrowPen, actionNode, actionNode_2)
    {
      this.arrowFont = arrowFont;
    }

    protected override void CalculateArrowEnd()
    {
      ArrowEnd = ArrowDst - ArrowSrc;
      ArrowEnd = ArrowEnd - (ArrowEnd.Normalize() * 15) + ArrowSrc;

      ArrowStart = ArrowSrc;
    }

    protected override void AdjustLinePoints(SelfConnector sc, int l, Point[] linePoints)
    {
      Vector2 t = new Vector2(linePoints[l - 1]) + sc.SymbolAdjustmentEnd;
      linePoints[l - 1] = t.ToPoint;
    }


    protected override void CalcArrowHeadSelfLoop(out Vector2 head1, out Vector2 head2)
    {
      head1 = ArrowEnd - ArrowSrc;
      head1 = (head1.Rotate(255).Normalize() * 20) + ArrowEnd;

      head2 = ArrowEnd - ArrowSrc;
      head2 = (head2.Rotate(205).Normalize() * 20) + ArrowEnd;
    }



    public override void Draw(Graphics g)
    {
        arrowBrush = Brushes.Red;
        arrowPen.Brush = arrowBrush;
      base.Draw(g);

      Vector2 arrowSymbol = ((ArrowDst - ArrowEnd) / 2) + ArrowEnd;

      g.DrawString("%", arrowFont, arrowBrush, new Point(arrowSymbol.ToPoint.X - 6, arrowSymbol.ToPoint.Y - 6));

    }

  }
}
