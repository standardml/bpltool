using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Drawing.Drawing2D;

namespace ITU.DK.DCRS.Visualization.Elements
{
  public class MilestoneArrow : Arrow
  {
      Vector2 headDirection;


    public MilestoneArrow(System.Drawing.Brush arrowBrush, System.Drawing.Pen arrowPen, ActionNode actionNode, ActionNode actionNode_2) 
      : base(arrowBrush, arrowPen, actionNode, actionNode_2)
    {
    }


    protected override void CalculateArrowEnd()
    {
        ArrowEnd = ArrowDst - ArrowSrc;
        ArrowEnd = ArrowEnd - (ArrowEnd.Normalize() * 25) + ArrowSrc;

        ArrowStart = ArrowSrc;
        headDirection = (ArrowDst - ArrowSrc).Normalize();
    }

    protected override void AdjustLinePoints(SelfConnector sc, int l, Point[] linePoints)
    {
        Vector2 t = new Vector2(linePoints[l - 1]) + (sc.SymbolAdjustmentEnd * 1.75f);
        linePoints[l - 1] = t.ToPoint;
    }


    protected override void CalcArrowHeadSelfLoop(out Vector2 head1, out Vector2 head2)
    {
        head1 = ArrowEnd - ArrowSrc;
        head1 = (head1.Rotate(265).Normalize() * 20) + ArrowEnd;

        head2 = ArrowEnd - ArrowSrc;
        head2 = (head2.Rotate(215).Normalize() * 20) + ArrowEnd;

        headDirection = (ArrowDst - ArrowSrc).Rotate(225-180).Normalize();
    }



    public override void Draw(Graphics g)
    {
        arrowBrush = Brushes.DarkMagenta;
        arrowPen.Brush = arrowBrush;
        base.Draw(g);
        if (stopDrawing)
            return;
        //g.FillEllipse(arrowBrush, ArrowDst.ToPoint.X - 3, ArrowDst.ToPoint.Y - 3, 6, 6);
        //g.DrawRectangle(arrowBrush)

        //(ArrowDst - ArrowSrc).
        System.Drawing.Point[] p = new System.Drawing.Point[4];
        //p[0] = (ArrowDst + new Vector2(5, 5).Rotate(45)).ToPoint;
        //p[1] = (ArrowDst + new Vector2(5, -5).Rotate(45)).ToPoint;
        //p[2] = (ArrowDst + new Vector2(-5, -5).Rotate(45)).ToPoint;
        //p[3] = (ArrowDst + new Vector2(-5, 5).Rotate(45)).ToPoint;

        float boxSize = 10f;

        p[0] = (ArrowEnd + (headDirection * boxSize) + (headDirection * boxSize).Rotate(0)).ToPoint;
        p[1] = (ArrowEnd + (headDirection * boxSize) + (headDirection * boxSize).Rotate(90)).ToPoint;
        p[2] = (ArrowEnd + (headDirection * boxSize) + (headDirection * boxSize).Rotate(180)).ToPoint;
        p[3] = (ArrowEnd + (headDirection * boxSize) + (headDirection * boxSize).Rotate(270)).ToPoint;

        g.DrawPolygon(arrowPen, p);
    }
  }
}
