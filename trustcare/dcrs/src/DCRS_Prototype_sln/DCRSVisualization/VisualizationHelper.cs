using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace ITU.DK.DCRS.Visualization
{
    /// <summary>
    /// Static class that contains some functions for helping in the visualization.
    /// </summary>
    public static class VisualizationHelper
    {
        /// <summary>
        /// Calculates the intersection of the line between point1 and point 2 and the line between point 3 and point4.
        /// </summary>
        /// <param name="point1"></param>
        /// <param name="point2"></param>
        /// <param name="point3"></param>
        /// <param name="point4"></param>
        /// <returns></returns>
        public static Point Intersection(Point point1, Point point2, Point point3, Point point4)
        {
            Point intersectionPoint = new Point(-100, -100);

            float ua = (point4.X - point3.X) * (point1.Y - point3.Y) - (point4.Y - point3.Y) * (point1.X - point3.X);
            float ub = (point2.X - point1.X) * (point1.Y - point3.Y) - (point2.Y - point1.Y) * (point1.X - point3.X);
            float denominator = (point4.Y - point3.Y) * (point2.X - point1.X) - (point4.X - point3.X) * (point2.Y - point1.Y);

            if (Math.Abs(denominator) <= 0.00001f)
            {
                if (Math.Abs(ua) <= 0.00001f && Math.Abs(ub) <= 0.00001f)
                {
                    //intersectionPoint = (point1 + point2) / 2;
                    intersectionPoint = new Point((point1.X + point2.X) / 2, (point1.Y + point2.Y) / 2);
                }
            }
            else
            {
                ua /= denominator;
                ub /= denominator;

                if (ua >= 0 && ua <= 1 && ub >= 0 && ub <= 1)
                {
                    intersectionPoint.X = (int)Math.Round(point1.X + ua * (point2.X - point1.X));
                    intersectionPoint.Y = (int)Math.Round(point1.Y + ua * (point2.Y - point1.Y));
                }
            }
                        
            return intersectionPoint;
        }

        /// <summary>
        /// Used in correspondance with the intersection method. Because Point is a non-nullable value, it returns -100, -100 if there was no intersection. 
        /// This should probably be improved upon...
        /// </summary>
        /// <param name="p"></param>
        /// <returns></returns>
        public static Boolean ValidPoint(Point p)
        {
          return (p.X != -100 && p.Y != -100);
        }

    }
}
