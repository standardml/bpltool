/// <summary>
/// The ActionNode class implements a visual node that represents an Action. 
/// It contains functionality for drawing the node itself and helper methods for drawing primitives between nodes.
/// </summary>
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Drawing.Drawing2D;

namespace ITU.DK.DCRS.Visualization.Elements
{

  /// <summary>
  /// Enumeration that represents the four sides of a node.
  /// </summary>
  public enum NodeSide { Top=0, Bottom, Left, Right, Corner };

  /// <summary>
  /// Nodeconnectors are single points on the edge of a node that primitives can connect to.
  /// </summary>
  public struct NodeConnector
  {
    public Vector2 Location;  /// Location of the node connector relative to the center of the node.
    public NodeSide Side;     /// The side of the node that the connector is on.
    public Boolean Used;      // !! remove this? !!
  }

  /// <summary>
  /// A special kind of NodeConnector that can be used for primitives that relate a node to itself.
  /// </summary>
  public struct SelfConnector
  {
    public Vector2[] Locations;         /// Several locations that can be used for drawing an arced line or arrow from the node to itself. All vectors are relative to the center of the node.
    public NodeSide Side;               /// The side of the node that the connector is on.
    public Boolean Used;                // !! Remove this? !!
    public Vector2 SymbolAdjustmentEnd; /// A vector that can be used for placing a symbol at the head of the arrow. Value is relative to the last vector in Locations.
    public Vector2 SymbolAdjustmentStart; /// A vector that can be used for placing a symbol at the head of the arrow. Value is relative to the last vector in Locations.
  }

  
  public class ActionNode
  {
    public const int NODE_WIDTH = 100;
    public const int NODE_HEIGHT = 100;
    public const int ROLEBOX_HEIGHT = 30;

    private int _nodeWidth = NODE_WIDTH;
    private int _nodeHeight = NODE_HEIGHT;
    private int _roleboxHeight = ROLEBOX_HEIGHT;

    private int _halfNodeWidth { get { return (_nodeWidth / 2); } }
    private int _halfNodeHeight { get { return (_nodeHeight / 2); } }

    public int Width { get { return (_nodeHeight); } }
    public int Height { get { return (_nodeHeight); } }

    short ID;                                 /// ID of this node in corresponding specification.
    String Name;                              /// Name of the action that this node represents.
    List<String> Roles;                       /// The roles that can execute the Action.
    public Vector2 Location;                  /// Location that the node should be drawn at. The vector represents the center of the node.
    Pen DrawingPen;                           
    Brush TextBrush;
    Font TextFont;
    Set<NodeConnector> UsedConnectors;        /// Set of connectors that have been used already.
    Set<NodeConnector> FreeConnectors;        /// Set of free connectors.
    Set<SelfConnector> UsedSelfConnectors;    /// Set of self-connectors that have been used already.
    Set<SelfConnector> FreeSelfConnectors;    /// Set of free self-connectors.
                                              /// 
    public short Id { get { return ID; } }

    /// Fields and/or properties for execution.
    public Boolean included = true;             /// Is the action currently included?
    public Boolean pendingResponse = false;     /// Is this action required to be performed as a response?      
    public Boolean hasExecuted = false;         /// Has this action been executed at least once?
    public Boolean enabled = false;             /// Is the action currently enabled?                                                
                                                /// 
    
      /// Fields for visualization related to the graphical editor
    public Boolean selected = false;             /// Is the action currently selected?

    int[] used = new int[4];


    // For subevents:
    private Set<ActionNode> subNodes;
    private Boolean isSuper { get { return subNodes.Count > 0; } }
    private Boolean isAtomic { get { return subNodes.Count == 0; } }
        
    public ActionNode(short id, String name, Vector2 location, Pen dp, Brush tb, Font tf)
    {
      Location = location;
      ID = id;
      Name = name;
      Roles = new List<String>();
      DrawingPen = dp;
      TextBrush = tb;
      TextFont = tf;
      InitiateConnectors();
      subNodes = new Set<ActionNode>();
    }

    public void ApplyRuntime(DCRS.CommonTypes.Process.DCRSRuntime r)
    {
        included = r.CurrentState.StateVector.IncludedActions.Contains(this.ID);
        pendingResponse = r.CurrentState.StateVector.PendingResponseActions.Contains(this.ID);
        hasExecuted = r.CurrentState.StateVector.ExecutedActions.Contains(this.ID);
        enabled = r.CurrentState.EnabledActions.Contains(this.ID);
    }

    // this should not be nessecairy, should reconsider the flow of drawing for nested nodes etc... but this will do for now.
    public void CalculateDimensions(){calculateDimensions();}

    // method for calculating nessecairy dimensions and location in the case that this is a super event (to contain all sub events)
    private void calculateDimensions()
    {
        Vector2 leftUp = new Vector2(99999,99999);
        Vector2 rightDown = new Vector2(0, 0);
        foreach (ActionNode a in subNodes)
        {

            leftUp.X = Math.Min(leftUp.X, a.Location.X);
            leftUp.Y = Math.Min(leftUp.Y, a.Location.Y);

            rightDown.X = Math.Max(rightDown.X, a.Location.X);
            rightDown.Y = Math.Max(rightDown.Y, a.Location.Y);
        }

        this._nodeWidth = (int)Math.Round(rightDown.X - leftUp.X) + NODE_WIDTH + NODE_WIDTH;
        this._nodeHeight = (int)Math.Round(rightDown.Y - leftUp.Y) + NODE_HEIGHT + NODE_HEIGHT;

        this.Location.X = (leftUp.X - (NODE_WIDTH)) + _halfNodeWidth;
        this.Location.Y = (leftUp.Y - (NODE_HEIGHT)) + _halfNodeHeight;

        // we need to also re-calculate connectors... // again... thsi could flow better?
        InitiateConnectors();
    }

    /// <summary>
    /// Method that creates all the (self-)connectors for this node and adds them to the set of free (self-)connectors.
    /// </summary>
    private void InitiateConnectors()
    {
      UsedConnectors = new Set<NodeConnector>();
      FreeConnectors = new Set<NodeConnector>();
      UsedSelfConnectors = new Set<SelfConnector>();
      FreeSelfConnectors = new Set<SelfConnector>();

      NodeConnector c;
      for (double d = 1; d < 8; d++)
      {
          c = new NodeConnector(); c.Location = new Vector2((_nodeWidth / -2), (_nodeHeight / -2) + (d * (_nodeHeight / 8))); c.Side = NodeSide.Left; c.Used = false; FreeConnectors.Add(c);
          c = new NodeConnector(); c.Location = new Vector2((_nodeWidth / 2), (_nodeHeight / -2) + (d * (_nodeHeight / 8))); c.Side = NodeSide.Right; c.Used = false; FreeConnectors.Add(c);
          c = new NodeConnector(); c.Location = new Vector2((_nodeWidth / -2) + (d * (_nodeWidth / 8)), (_nodeHeight / 2)); c.Side = NodeSide.Bottom; c.Used = false; FreeConnectors.Add(c);
      }

      
      for (double d = 1; d < 4; d++)
      {
          c = new NodeConnector(); c.Location = new Vector2((_nodeWidth / -2) + (d * (_nodeWidth / 8)), (_nodeHeight / -2)); c.Side = NodeSide.Top; c.Used = false; FreeConnectors.Add(c);
      }

      for (double d = 5; d < 8; d++)
      {
          c = new NodeConnector(); c.Location = new Vector2((_nodeWidth / -2) + (d * (_nodeWidth / 8)), (_nodeHeight / -2) - _roleboxHeight); c.Side = NodeSide.Top; c.Used = false; FreeConnectors.Add(c);
      }


      for (double d = 0; d < 2; d++)
      {
          c = new NodeConnector(); c.Location = new Vector2((_nodeWidth / 2), (_nodeHeight / -2) - (d * (_roleboxHeight / 2))); c.Side = NodeSide.Right; c.Used = false; FreeConnectors.Add(c);
      }


      // unfinsihed - see below as well:
      SelfConnector s;
      s = new SelfConnector();
      s.Side = NodeSide.Right;
      s.Used = false;
      s.Locations = new Vector2[3] { new Vector2((_nodeWidth / 2), (_nodeWidth / -2) - _roleboxHeight), new Vector2((_nodeWidth / 2) * 2, _roleboxHeight / -2), new Vector2((_nodeWidth / 2), (_nodeWidth / 2)) };
      s.SymbolAdjustmentEnd = new Vector2(10, -10);
      FreeSelfConnectors.Add(s);

    }

    /// <summary>
    /// Adds a role to the ActionNode
    /// </summary>
    public void AddRole(String role)
    {
      Roles.Add(role);
    }

    /// <summary>
    /// Adds a subNode to the ActionNode
    /// </summary>
    public void AddSubNode(ActionNode subNode)
    {
        subNodes.Add(subNode);
    }

    /// <summary>
    /// Sets a given list of roles as the roles for this ActionNode.
    /// </summary>
    public void SetRoles(List<String> roles)
    {
      Roles = roles;
    }

    /// <summary>
    /// Draws the node.
    /// </summary>
    /// <param name="g"></param>
    public void Draw(Graphics g)
    {
        if (!isAtomic) calculateDimensions();
        if (!included) DrawingPen.DashStyle = DashStyle.Dash;
        if (selected) DrawingPen.Color = Color.DarkTurquoise;
        g.DrawRectangle(DrawingPen, Location.ToPoint.X - (_nodeWidth / 2), Location.ToPoint.Y - (_nodeHeight / 2), _nodeWidth, _nodeHeight);        
        //g.DrawString(Name, TextFont, TextBrush, Location.ToPoint);
        StringFormat sf = new StringFormat();
        if (isAtomic)
        {
            sf.Alignment = StringAlignment.Center;
            sf.LineAlignment = StringAlignment.Center;
        }
        else
        {
            sf.Alignment = StringAlignment.Far;
            sf.LineAlignment = StringAlignment.Near;
        }
        g.DrawString(Name, TextFont, TextBrush, new Rectangle((int)((Location.X - (_nodeWidth / 2)) + 5), (int)((Location.Y - (_nodeHeight / 2)) + 5), _nodeWidth - 10, _nodeHeight - 10), sf);



        g.DrawRectangle(DrawingPen, Location.ToPoint.X, Location.ToPoint.Y - ((_nodeHeight / 2) + _roleboxHeight), (_nodeWidth / 2), _roleboxHeight);

        if (!included) DrawingPen.DashStyle = DashStyle.Solid;
        if (selected) DrawingPen.Color = Color.Black;
        

        string rString = "";
        //int offset = 0;
        foreach (String r in Roles)
        {
        //g.DrawString(r, TextFont, TextBrush, Location.ToPoint.X, (Location.ToPoint.Y - (80 - (offset * 5))));
        //offset++;
            rString += r + ", ";
        }

        if (rString != "") rString = rString.Substring(0, rString.Length - 2);
        g.DrawString(rString, TextFont, TextBrush, new Rectangle((int)(Location.X + 1), (int)(Location.Y - ((_nodeHeight / 2) + (_roleboxHeight - 1))), (_nodeWidth / 2)-2, _roleboxHeight - 2), sf);

        if (isAtomic)
        {
            Font BoldFont = new Font(FontFamily.GenericSansSerif, 14f, FontStyle.Bold);
            TextBrush = Brushes.Red;
            //TextFont = new Font(FontFamily.GenericSansSerif, 14f, FontStyle.Bold);      
            if (pendingResponse) g.DrawString("!", BoldFont, TextBrush, (Location + new Vector2(35, -45)).ToPoint);

            TextBrush = Brushes.Green;
            //TextFont = new Font(FontFamily.GenericSansSerif, 14f, FontStyle.Bold);      
            if (hasExecuted) g.DrawString("V", BoldFont, TextBrush, (Location + new Vector2(-48, -45)).ToPoint);
            TextBrush = Brushes.Black;

            DrawingPen.Brush = Brushes.Red;
            if (included && !enabled)
            {
                g.DrawEllipse(DrawingPen, Location.ToPoint.X - 30, Location.ToPoint.Y - 47, 20, 20);
                g.DrawLine(DrawingPen, (Location + new Vector2(-20, -37) + new Vector2(7, -7)).ToPoint, (Location + new Vector2(-20, -37) + new Vector2(-7, 7)).ToPoint);
            }
        }
        DrawingPen.Brush = Brushes.Black;


        
    }

    /// <summary>
    /// Returns the closest free connector to a certain vertor s.
    /// </summary>
    /// <param name="s"></param>
    /// <returns></returns>
    public NodeConnector ClosestFreeConnector(Vector2 s)
    {
      double minL = 99999;
      NodeConnector minN = new NodeConnector();
      foreach (NodeConnector n in FreeConnectors)
      {
        if (((n.Location+Location) - s).Magnitude < minL)
        {
          minN = n;
          minL = ((n.Location + Location) - s).Magnitude;
        }
      }

      return minN;
    }


    // this can be done much better...
    public SelfConnector NextSelfConnector()
    {

      int minUsed = 99999;

      foreach (int i in used)
      {
        minUsed = Math.Min(minUsed, i);
      }

      // arrow angles should probably also be settable...
      if (used[(int)NodeSide.Right] == minUsed)                 
      {
        SelfConnector s;
        s = new SelfConnector();
        s.Side = NodeSide.Right;
        s.Used = false;
        s.Locations = new Vector2[3] { new Vector2((_nodeWidth / 2), (_nodeWidth / -2) - _roleboxHeight), new Vector2((_nodeWidth / 2) * 2, _roleboxHeight / -2), new Vector2((_nodeWidth / 2), (_nodeWidth / 2)) };
        s.SymbolAdjustmentEnd = new Vector2(10, -10);
        s.SymbolAdjustmentStart = new Vector2(15, 15);
        used[(int)s.Side] += 999;
        return s;
      }

      if (used[(int)NodeSide.Left] == minUsed)      
      {
        SelfConnector s;
        s = new SelfConnector();
        s.Side = NodeSide.Left;
        s.Used = false;
        s.Locations = new Vector2[3] { new Vector2((_nodeWidth / -2), (_nodeWidth / 2)), new Vector2((_nodeWidth / -2) * 2, 0), new Vector2((_nodeWidth / -2), (_nodeWidth / -2)) };
        s.SymbolAdjustmentEnd = new Vector2(-10, 10);
        s.SymbolAdjustmentStart = new Vector2(-10, -10);
        used[(int)s.Side] += 999;
        return s;
      }

      if (used[(int)NodeSide.Bottom] == minUsed)      
      {
        SelfConnector s;
        s = new SelfConnector();
        s.Side = NodeSide.Bottom;
        s.Used = false;
        s.Locations = new Vector2[3] { new Vector2((_nodeWidth / 2), (_nodeWidth / 2)), new Vector2(0, (_nodeWidth / 2) * 2), new Vector2((_nodeWidth / -2), (_nodeWidth / 2)) };
        s.SymbolAdjustmentEnd = new Vector2(10, 10);
        s.SymbolAdjustmentStart = new Vector2(-10, 10);
        used[(int)s.Side] += 999;
        return s;
      }

      if (used[(int)NodeSide.Top] == minUsed)      
      {
        SelfConnector s;
        s = new SelfConnector();
        s.Side = NodeSide.Top;
        s.Used = false;
        s.Locations = new Vector2[3] { new Vector2((_nodeWidth / -2), (_nodeWidth / -2)), new Vector2(0, (_nodeWidth / 2) * -2), new Vector2((_nodeWidth / 2), (_nodeWidth / -2) - _roleboxHeight) };
        s.SymbolAdjustmentEnd = new Vector2(-10, -10);
        s.SymbolAdjustmentStart = new Vector2(10, -10);
        used[(int)s.Side] += 999;
        return s;
      }

      throw new Exception("invalid execution path");

    }

    /// <summary>
    /// Calculates the point at which the rectangle part of this node intersects with the line going from src to dst.
    /// </summary>
    /// <param name="src"></param>
    /// <param name="dst"></param>
    /// <returns>The point of intersection. Throws an exception incase no such point exists.</returns>
    public Point RectIntersect(Point src, Point dst)
    {
        Point topRight = new Point(Location.ToPoint.X + _halfNodeWidth, Location.ToPoint.Y - _halfNodeHeight);
        Point bottomRight = new Point(Location.ToPoint.X + _halfNodeWidth, Location.ToPoint.Y + _halfNodeHeight);

        Point topLeft = new Point(Location.ToPoint.X - _halfNodeWidth, Location.ToPoint.Y - _halfNodeHeight);
        Point bottomLeft = new Point(Location.ToPoint.X - _halfNodeWidth, Location.ToPoint.Y + _halfNodeHeight);

        Point Right = VisualizationHelper.Intersection(topRight, bottomRight, src, dst);
        Point Left = VisualizationHelper.Intersection(topLeft, bottomLeft, src, dst);
        Point Top = VisualizationHelper.Intersection(topRight, topLeft, src, dst);
        Point Bottom = VisualizationHelper.Intersection(bottomLeft, bottomRight, src, dst);

        if (VisualizationHelper.ValidPoint(Right))
        {
            if (VisualizationHelper.ValidPoint(Bottom))
                return bottomRight;
            else if (VisualizationHelper.ValidPoint(Top))
                return topRight;
            else return Right;
        }
        else if (VisualizationHelper.ValidPoint(Left))
        {
            if (VisualizationHelper.ValidPoint(Bottom))
                return bottomLeft;
            else if (VisualizationHelper.ValidPoint(Top))
                return topLeft;
            else return Left;
        }
        else if (VisualizationHelper.ValidPoint(Top))
        {
            // already covered the corner exceptions
            return Top;
        }
        else if (VisualizationHelper.ValidPoint(Bottom))
        {
            // already covered the corner exceptions
            return Bottom;
        }
        throw new Exception("Invalid execution point - there should always be an intersection between a rectangle and a line going from it's center to some point outside the rectangle!");
    }



    /// <summary>
    /// Mocvs a connector from the set of free connectors to the set of used connectors.
    /// </summary>
    /// <param name="ncSrc"></param>
    internal void LockConnector(NodeConnector ncSrc)
    {
        used[(int)ncSrc.Side]++;
        FreeConnectors.Remove(ncSrc);
        //ncSrc.Used = true;
        UsedConnectors.Add(ncSrc);
    }


    public Boolean PointInNode(Point p)
    {
        if (p.X < Location.X + _halfNodeWidth && p.X > Location.X - _halfNodeWidth)
            if (p.Y < Location.Y + _halfNodeHeight && p.Y > Location.Y - _halfNodeHeight)
                return true;
        return false;
    }


    public Point ClosestEdge(ActionNode n)
    {
        float horizontalLeft = Math.Abs((Location.ToPoint.X + _halfNodeWidth) - (n.Location.ToPoint.X + (n.Width / 2)));
        float horizontalRight = Math.Abs((Location.ToPoint.X - _halfNodeWidth) - (n.Location.ToPoint.X - (n.Width / 2)));

        float verticalUp = Math.Abs((Location.ToPoint.Y - _halfNodeHeight) - (n.Location.ToPoint.Y - (n.Height / 2)));
        float verticalDown = Math.Abs((Location.ToPoint.Y + _halfNodeHeight) - (n.Location.ToPoint.Y + (n.Height / 2)));

        float min = Math.Min(horizontalLeft, horizontalRight);
        min = Math.Min(min, verticalUp);
        min = Math.Min(min, verticalDown);

        /*
        if (min == horizontalLeft) 
            return new Point((n.Location.ToPoint.X + (n.Width / 2)), n.Location.ToPoint.Y);
        else if (min == horizontalRight) 
            return new Point((n.Location.ToPoint.X - (n.Width / 2)), n.Location.ToPoint.Y);
        else if (min == verticalUp)
            return new Point(n.Location.ToPoint.X, (n.Location.ToPoint.Y - (n.Height / 2)));
        else
            return new Point(n.Location.ToPoint.X, (n.Location.ToPoint.Y + (n.Height / 2)));*/


        if (min == horizontalLeft) 
            return new Point((Location.ToPoint.X + (Width / 2)), Location.ToPoint.Y);
        else if (min == horizontalRight) 
            return new Point((Location.ToPoint.X - (Width / 2)), Location.ToPoint.Y);
        else if (min == verticalUp)
            return new Point(Location.ToPoint.X, (Location.ToPoint.Y - (Height / 2)));
        else
            return new Point(Location.ToPoint.X, (Location.ToPoint.Y + (Height / 2)));
    }
  }
}
