﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Drawing.Drawing2D;


namespace ITU.DK.DCRS.Visualization.Elements
{
  /// <summary>
  /// Base class for drawing arrows.
  /// </summary>
  public class Arrow
  {
    protected System.Drawing.Brush arrowBrush;
    protected System.Drawing.Pen arrowPen;
    protected ActionNode srcNode;
    protected ActionNode dstNode;

    public Arrow(System.Drawing.Brush arrowBrush, System.Drawing.Pen arrowPen, ActionNode actionNode, ActionNode actionNode_2)
    {
      this.arrowBrush = arrowBrush;
      this.arrowPen = arrowPen;
      this.srcNode = actionNode;
      this.dstNode = actionNode_2;
      //Init();
    }

    protected Vector2 ArrowSrc; // The source of the arrow
    protected Vector2 ArrowDst; // The destination of the arrow
    protected Vector2 ArrowEnd; // The arrowtip, excluding any symbols ahead of the arrowtip.
    protected Vector2 ArrowStart; // The arrow starting point, excluding any symbols at the start of the arrow.

    public Point ArrowSource { get { return ArrowSrc.ToPoint; } }
    public Point ArrowDestination { get { return ArrowDst.ToPoint; } }

    public ActionNode SourceNode { get { return srcNode; } }
    public ActionNode DestinationNode { get { return dstNode; } }


    public void Init()
    {
        if (srcNode == dstNode)
            InitSelf();
        else
            InitOther();
    }

    protected void InitOther()
    {
        FindBestConnectors();   // really needs to be made cleaner...
    }


    protected SelfConnector selfConnector;
    protected void InitSelf()
    {
        selfConnector = srcNode.NextSelfConnector();
    }


    /// <summary>
    /// Main drawing method, makes a distinction between arrows that loopback to the same node, or arrows between different nodes.
    /// </summary>
    /// <param name="g"></param>
    public virtual void Draw(Graphics g)
    {
      if (srcNode == dstNode)
        DrawSelf(g);
      else
        DrawOther(g);
    }

    /// <summary>
    /// Method for drawing an arrow that loops back on the same node.
    /// </summary>
    /// <param name="g"></param>
    private void DrawSelf(Graphics g)
    {
        SelfConnector sc = selfConnector;

        int l = sc.Locations.Length;
        Point[] linePoints = new Point[l];


        for (int i = 0; i < linePoints.Length; i++)
        linePoints[i] = (srcNode.Location + sc.Locations[i]).ToPoint;

        ArrowDst = sc.Locations[l - 1] + srcNode.Location;
        ArrowSrc = sc.Locations[0] + srcNode.Location;

        AdjustLinePoints(sc, l, linePoints);  
        g.DrawCurve(arrowPen, linePoints);
      
        ArrowStart = new Vector2(linePoints[0]);
        ArrowEnd = new Vector2(linePoints[l - 1]);

        Vector2 head1;
        Vector2 head2;
        CalcArrowHeadSelfLoop(out head1, out head2);


        g.DrawLine(arrowPen, ArrowEnd.ToPoint, head1.ToPoint);
        g.DrawLine(arrowPen, ArrowEnd.ToPoint, head2.ToPoint);
    }

    /// <summary>
    /// Calculates vectors for drawing the head of the arrow in the case where DrawSelf is called.
    /// </summary>
    /// <param name="head1"></param>
    /// <param name="head2"></param>
    protected virtual void CalcArrowHeadSelfLoop(out Vector2 head1, out Vector2 head2)
    {
      head1 = ArrowEnd - ArrowSrc;
      head1 = (head1.Rotate(250).Normalize() * 20) + ArrowEnd;

      head2 = ArrowEnd - ArrowSrc;
      head2 = (head2.Rotate(200).Normalize() * 20) + ArrowEnd;
    }
    
    /// <summary>
    /// Method that can be overridden to adjust the linepoints for a self-looping arrow.
    /// </summary>
    /// <param name="sc"></param>
    /// <param name="l"></param>
    /// <param name="linePoints"></param>
    protected virtual void AdjustLinePoints(SelfConnector sc, int l, Point[] linePoints)
    {
      //skip
    }

    protected bool stopDrawing = false;

    /// <summary>
    ///  Method for drawing an arrow between two different nodes.
    /// </summary>
    /// <param name="g"></param>
    public void DrawOther(Graphics g)
    {
      //FindBestConnectors();
        if (ArrowDst.Equals(ArrowSrc))
        {
            stopDrawing = true;
            return;
        }

        CalculateArrowEnd();

        Vector2 head1;
        Vector2 head2;
        CalculateArrowHead(out head1, out head2);

        DrawStraightArrow(g, head1, head2);
      
    }

    /// <summary>
    /// Drawing calls for drawing a straight arrow.
    /// </summary>
    /// <param name="g"></param>
    /// <param name="head1"></param>
    /// <param name="head2"></param>
    private void DrawStraightArrow(Graphics g, Vector2 head1, Vector2 head2)
    {
      g.DrawLine(arrowPen, ArrowStart.ToPoint, ArrowEnd.ToPoint);
      g.DrawLine(arrowPen, ArrowEnd.ToPoint, head1.ToPoint);
      g.DrawLine(arrowPen, ArrowEnd.ToPoint, head2.ToPoint);
    }


    protected void DrawStartHead(Graphics g)
    {
        Vector2 head1;
        Vector2 head2;
        if (srcNode == dstNode)
        {
            // This should be overridable at somepoint (be in its own function), but for now there is only one arrow type using it anyway....
            head1 = ArrowEnd - ArrowSrc;
            head1 = (head1.Rotate(345).Normalize() * 20) + ArrowStart;

            head2 = ArrowEnd - ArrowSrc;
            head2 = (head2.Rotate(285).Normalize() * 20) + ArrowStart;
        }
        else
        {
            head1 = ArrowDst - ArrowSrc;
            head1 = (head1.Rotate(-20).Normalize() * 20) + ArrowStart;

            head2 = ArrowDst - ArrowSrc;
            head2 = (head2.Rotate(20).Normalize() * 20) + ArrowStart;

        }
            
        g.DrawLine(arrowPen, ArrowStart.ToPoint, head1.ToPoint);
        g.DrawLine(arrowPen, ArrowStart.ToPoint, head2.ToPoint);
    }


    /// <summary>
    /// Calculates vectors for drawing the head of the arrow in the case where DrawOther is called.
    /// </summary>
    /// <param name="head1"></param>
    /// <param name="head2"></param>
    private void CalculateArrowHead(out Vector2 head1, out Vector2 head2)
    {
      head1 = ArrowDst - ArrowSrc;
      head1 = (head1.Rotate(160).Normalize() * 20) + ArrowEnd;

      head2 = ArrowDst - ArrowSrc;
      head2 = (head2.Rotate(-160).Normalize() * 20) + ArrowEnd;
    }

    /// <summary>
    /// Calculates the intended arrowend when DrawOther is called.
    /// </summary>
    protected virtual void CalculateArrowEnd()
    {
      ArrowEnd = ArrowDst;
      ArrowStart = ArrowSrc;
    }

    /// <summary>
    /// Finds the best connectors in the case where DrawOther is called.
    /// </summary>
    private void FindBestConnectors()
    {
      NodeConnector toLockSrc;
      NodeConnector toLockDst;

      Vector2 v;
      try
      {
          if (srcNode.PointInNode(dstNode.Location.ToPoint))
              v = new Vector2(srcNode.ClosestEdge(dstNode));
          else
              v = new Vector2(srcNode.RectIntersect(srcNode.Location.ToPoint, dstNode.Location.ToPoint));
      }
      catch (Exception e)
      {
          ArrowDst = ArrowSrc = new Vector2(0,0);
          return;
      }
      NodeConnector ncSrc1 = srcNode.ClosestFreeConnector(v);

      Vector2 arrowSrc1 = ncSrc1.Location + srcNode.Location;

      NodeConnector ncDst1 = dstNode.ClosestFreeConnector(arrowSrc1);
      Vector2 arrowDst1 = ncDst1.Location + dstNode.Location;


      try
      {
          if (dstNode.PointInNode(srcNode.Location.ToPoint))
              v = new Vector2(dstNode.ClosestEdge(srcNode));
          else
              v = new Vector2(dstNode.RectIntersect(dstNode.Location.ToPoint, srcNode.Location.ToPoint));
      }
      catch (Exception e)
      {
          ArrowDst = ArrowSrc = new Vector2(0, 0);
          return;
      }
      NodeConnector ncDst2 = dstNode.ClosestFreeConnector(v);
      Vector2 arrowDst2 = ncDst2.Location + dstNode.Location;

      NodeConnector ncSrc2 = srcNode.ClosestFreeConnector(arrowDst2);
      Vector2 arrowSrc2 = ncSrc2.Location + srcNode.Location;


      if ((arrowSrc2 - arrowDst2).Magnitude < (arrowSrc1 - arrowDst1).Magnitude)
      {
        ArrowSrc = arrowSrc2;
        ArrowDst = arrowDst2;
        toLockSrc = ncSrc2;
        toLockDst = ncDst2;
        //srcNode.LockConnector(ncSrc2);
        //dstNode.LockConnector(ncDst2);
      }
      else
      {
        ArrowSrc = arrowSrc1;
        ArrowDst = arrowDst1;
        toLockSrc = ncSrc1;
        toLockDst = ncDst1;
        //srcNode.LockConnector(ncSrc1);
        //dstNode.LockConnector(ncDst1);
      }

      bool checkmore = true;

      while (checkmore)
      {
        NodeConnector altDst = dstNode.ClosestFreeConnector(ArrowSrc);
        NodeConnector altSrc = srcNode.ClosestFreeConnector(ArrowDst);

        Vector2 altDstVec = altDst.Location + dstNode.Location;
        Vector2 altSrcVec = altSrc.Location + srcNode.Location;

        if ((ArrowSrc - altDstVec).Magnitude < (ArrowDst - altSrcVec).Magnitude)
        {
          if ((ArrowSrc - altDstVec).Magnitude < (ArrowDst - ArrowSrc).Magnitude)
          {
            ArrowDst = altDstVec;
            toLockDst = altDst;
          }
          else
            checkmore = false;
        }
        else
        {
          if ((altSrcVec - ArrowDst).Magnitude < (ArrowDst - ArrowSrc).Magnitude)
          {
            ArrowSrc = altSrcVec;
            toLockSrc = altSrc;
          }
          else
            checkmore = false;
        }
      }

      srcNode.LockConnector(toLockSrc);
      dstNode.LockConnector(toLockDst);
    }

    public double StraightDistance(Point clicked)
    {
        Vector2 a = this.ArrowSrc;
        Vector2 b = this.ArrowDst;
        Vector2 c = new Vector2(clicked);
        double u = ((c.X - a.X) * (b.X - a.X) + (c.Y - a.Y) * (b.Y - a.Y)) / ((b - a).Length() * (b - a).Length());

        Vector2 t = new Vector2(a.X + u * (b.X - a.X), a.Y + u * (b.Y - a.Y));

        if (!(t.X <= Math.Max(a.X, b.X) && t.X >= Math.Min(a.X, b.X) && t.Y <= Math.Max(a.Y, b.Y) && t.Y >= Math.Min(a.Y, b.Y)))
            return Math.Min((c - a).Length(), (c - b).Length());
        else
            return (t - c).Length();
    }


    public bool PointWithinSelfConnectorBoudningBox(Point clicked)
    {
        double maxX = this.selfConnector.Locations[0].X;
        double maxY = this.selfConnector.Locations[0].Y;
        double minX = this.selfConnector.Locations[0].X;
        double minY = this.selfConnector.Locations[0].Y;
        foreach (Vector2 v in this.selfConnector.Locations)
        {
            maxX = Math.Max(maxX, v.X);
            maxY = Math.Max(maxY, v.Y);
            minX = Math.Min(minX, v.X);
            minY = Math.Min(minY, v.Y);
        }

        maxX += this.srcNode.Location.X;
        minX += this.srcNode.Location.X;
        maxY += this.srcNode.Location.Y;
        minY += this.srcNode.Location.Y;

        return (clicked.X < maxX && clicked.Y < maxY && clicked.X > minX && clicked.Y > minY);
    }

  }
}
