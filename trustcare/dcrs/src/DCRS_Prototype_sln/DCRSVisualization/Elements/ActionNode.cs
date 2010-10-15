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
    static public int NODE_WIDTH = 100;
    static public int NODE_HEIGHT = 100;
    static public int ROLEBOX_HEIGHT = 30;

    static public int HALF_NODE_WIDTH = (NODE_WIDTH/2);
    static public int HALF_NODE_HEIGHT = (NODE_HEIGHT/2);


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

    /// Fields and/or properties for execution.
    public Boolean included = true;             /// Is the action currently included?
    public Boolean pendingResponse = false;     /// Is this action required to be performed as a response?      
    public Boolean hasExecuted = false;         /// Has this action eben executed at least one?
    public Boolean enabled = false;             /// Is the action currently enabled?                                                

    int[] used = new int[4];
        
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
    }

    public void ApplyRuntime(DCRS.CommonTypes.Process.DCRSRuntime r)
    {
        included = r.CurrentState.StateVector.IncludedActions.Contains(this.ID);
        pendingResponse = r.CurrentState.StateVector.PendingResponseActions.Contains(this.ID);
        hasExecuted = r.CurrentState.StateVector.ExecutedActions.Contains(this.ID);
        enabled = r.CurrentState.EnabledActions.Contains(this.ID);
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
        c = new NodeConnector(); c.Location = new Vector2((NODE_WIDTH / -2), (NODE_WIDTH / -2) + (d * (NODE_WIDTH / 8))); c.Side = NodeSide.Left; c.Used = false; FreeConnectors.Add(c);
        c = new NodeConnector(); c.Location = new Vector2((NODE_WIDTH / 2), (NODE_WIDTH / -2) + (d * (NODE_WIDTH / 8))); c.Side = NodeSide.Right; c.Used = false; FreeConnectors.Add(c);
        c = new NodeConnector(); c.Location = new Vector2((NODE_WIDTH / -2) + (d * (NODE_WIDTH / 8)), (NODE_WIDTH / 2)); c.Side = NodeSide.Bottom; c.Used = false; FreeConnectors.Add(c);
      }

      
      for (double d = 1; d < 4; d++)
      {
        c = new NodeConnector(); c.Location = new Vector2((NODE_WIDTH / -2) + (d * (NODE_WIDTH / 8)), (NODE_WIDTH / -2)); c.Side = NodeSide.Top; c.Used = false; FreeConnectors.Add(c);
      }

      for (double d = 5; d < 8; d++)
      {
        c = new NodeConnector(); c.Location = new Vector2((NODE_WIDTH / -2) + (d * (NODE_WIDTH / 8)), (NODE_WIDTH / -2) - ROLEBOX_HEIGHT); c.Side = NodeSide.Top; c.Used = false; FreeConnectors.Add(c);
      }


      for (double d = 0; d < 2; d++)
      {
        c = new NodeConnector(); c.Location = new Vector2((NODE_WIDTH / 2), (NODE_WIDTH / -2) - (d * (ROLEBOX_HEIGHT / 2))); c.Side = NodeSide.Right; c.Used = false; FreeConnectors.Add(c);
      }


      // unfinsihed - see below as well:
      SelfConnector s;
      s = new SelfConnector();
      s.Side = NodeSide.Right;
      s.Used = false;
      s.Locations = new Vector2[3] { new Vector2((NODE_WIDTH / 2), (NODE_WIDTH / -2) - ROLEBOX_HEIGHT), new Vector2((NODE_WIDTH / 2) * 2, ROLEBOX_HEIGHT / -2), new Vector2((NODE_WIDTH / 2), (NODE_WIDTH / 2)) };
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
        if (!included) DrawingPen.DashStyle = DashStyle.Dash;
        g.DrawRectangle(DrawingPen, Location.ToPoint.X - (NODE_WIDTH / 2), Location.ToPoint.Y - (NODE_HEIGHT / 2), NODE_WIDTH, NODE_HEIGHT);
        //g.DrawString(Name, TextFont, TextBrush, Location.ToPoint);
        StringFormat sf = new StringFormat();
        sf.Alignment = StringAlignment.Center;
        sf.LineAlignment = StringAlignment.Center;
        g.DrawString(Name, TextFont, TextBrush, new Rectangle((int)(Location.X - 45), (int)(Location.Y - 45), 90, 90), sf);
        

        g.DrawRectangle(DrawingPen, Location.ToPoint.X, Location.ToPoint.Y - 80, 50, ROLEBOX_HEIGHT);

        if (!included) DrawingPen.DashStyle = DashStyle.Solid;

        string rString = "";
        //int offset = 0;
        foreach (String r in Roles)
        {
        //g.DrawString(r, TextFont, TextBrush, Location.ToPoint.X, (Location.ToPoint.Y - (80 - (offset * 5))));
        //offset++;
            rString += r + ", ";
        }

        rString = rString.Substring(0, rString.Length - 2);
        g.DrawString(rString, TextFont, TextBrush, new Rectangle((int)(Location.X + 1), (int)(Location.Y - 79), 48, 28), sf);


        Font BoldFont = new Font(FontFamily.GenericSansSerif, 14f, FontStyle.Bold);      
        TextBrush = Brushes.Red;
        //TextFont = new Font(FontFamily.GenericSansSerif, 14f, FontStyle.Bold);      
        if (pendingResponse) g.DrawString("!", BoldFont, TextBrush, (Location + new Vector2(35, -45)).ToPoint);

        TextBrush = Brushes.Green;
        //TextFont = new Font(FontFamily.GenericSansSerif, 14f, FontStyle.Bold);      
        if (hasExecuted) g.DrawString("V", BoldFont, TextBrush, (Location + new Vector2(-48, -45)).ToPoint);
        TextBrush = Brushes.Black;

        DrawingPen.Brush = Brushes.Red;
        if (!enabled)
        {
            g.DrawEllipse(DrawingPen, Location.ToPoint.X - 30, Location.ToPoint.Y - 47, 20,20);
            g.DrawLine(DrawingPen, (Location + new Vector2(-20, -37) + new Vector2(7, -7)).ToPoint, (Location + new Vector2(-20, -37) + new Vector2(-7, 7)).ToPoint);
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

      int minUsed = 999;

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
        s.Locations = new Vector2[3] { new Vector2((NODE_WIDTH / 2), (NODE_WIDTH / -2) - ROLEBOX_HEIGHT), new Vector2((NODE_WIDTH / 2) * 2, ROLEBOX_HEIGHT / -2), new Vector2((NODE_WIDTH / 2), (NODE_WIDTH / 2)) };
        s.SymbolAdjustmentEnd = new Vector2(10, -10);
        s.SymbolAdjustmentStart = new Vector2(15, 15);
        return s;
      }

      if (used[(int)NodeSide.Left] == minUsed)      
      {
        SelfConnector s;
        s = new SelfConnector();
        s.Side = NodeSide.Left;
        s.Used = false;
        s.Locations = new Vector2[3] { new Vector2((NODE_WIDTH / -2), (NODE_WIDTH / 2)), new Vector2((NODE_WIDTH / -2) * 2, 0), new Vector2((NODE_WIDTH / -2), (NODE_WIDTH / -2)) };
        s.SymbolAdjustmentEnd = new Vector2(-10, 10);
        s.SymbolAdjustmentStart = new Vector2(-10, -10);
        return s;
      }

      if (used[(int)NodeSide.Bottom] == minUsed)      
      {
        SelfConnector s;
        s = new SelfConnector();
        s.Side = NodeSide.Bottom;
        s.Used = false;
        s.Locations = new Vector2[3] { new Vector2((NODE_WIDTH / 2), (NODE_WIDTH / 2)), new Vector2(0, (NODE_WIDTH / 2) * 2), new Vector2((NODE_WIDTH / -2), (NODE_WIDTH / 2)) };
        s.SymbolAdjustmentEnd = new Vector2(10, 10);
        s.SymbolAdjustmentStart = new Vector2(-10, 10);
        return s;
      }

      if (used[(int)NodeSide.Top] == minUsed)      
      {
        SelfConnector s;
        s = new SelfConnector();
        s.Side = NodeSide.Top;
        s.Used = false;
        s.Locations = new Vector2[3] { new Vector2((NODE_WIDTH / -2), (NODE_WIDTH / -2)), new Vector2(0, (NODE_WIDTH / 2) * -2), new Vector2((NODE_WIDTH / 2), (NODE_WIDTH / -2) - ROLEBOX_HEIGHT) };
        s.SymbolAdjustmentEnd = new Vector2(-10, -10);
        s.SymbolAdjustmentStart = new Vector2(10, -10);
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
        Point topRight = new Point(Location.ToPoint.X + HALF_NODE_WIDTH, Location.ToPoint.Y - HALF_NODE_HEIGHT);
        Point bottomRight = new Point(Location.ToPoint.X + HALF_NODE_WIDTH, Location.ToPoint.Y + HALF_NODE_HEIGHT);

        Point topLeft = new Point(Location.ToPoint.X - HALF_NODE_WIDTH, Location.ToPoint.Y - HALF_NODE_HEIGHT);
        Point bottomLeft = new Point(Location.ToPoint.X - HALF_NODE_WIDTH, Location.ToPoint.Y + HALF_NODE_HEIGHT);

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
  }
}
