using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace ITU.DK.DCRS.Visualization
{
    /// <summary>
    /// A class for describing 2-dimensional vectors.
    /// Functionalitty is fairly incomplete, features are added as required.
    /// </summary>
    public class Vector2
    {
        public double X;
        public double Y;

        public Vector2()
        {
          X = 0;
          Y = 0;
        }


        public Vector2(Point p)
        {
            X = p.X;
            Y = p.Y;
        }

        public Vector2(Double d1, Double d2)
        {
            X = d1;
            Y = d2;
        }


        public Point ToPoint { get { return new Point((int)Math.Round(X), (int)Math.Round(Y)); } }


        /// <summary>
        /// Calculates the magnitude of the vector.
        /// </summary>
        public double Magnitude { get { return Math.Sqrt((X*X) + (Y*Y)); } }

        /// <summary>
        /// Same as the magnitude property, but now as a function. (Mainly included for making it easier to plug in some old code).
        /// </summary>
        public double Length() { return Magnitude; }
        
        /// <summary>
        /// Returns the magnitude/length of the vector as a float.
        /// </summary>
        /// <returns></returns>
        public float fLength() { return (float)Magnitude; } 

        /// <summary>
        /// Static method for normalizing any vector
        /// </summary>
        /// <param name="v1"></param>
        /// <returns></returns>
        public static Vector2 Normalize(Vector2 v1)
        {
            // Check for divide by zero errors

            if (v1.Magnitude == 0)
            {
                throw new DivideByZeroException("Attempting to normalize 0-vector...");
            }
            else
            {
                // find the inverse of the vectors magnitude

                double inverse = 1 / v1.Magnitude;
                return
                (
                   new Vector2
                   (
                    // multiply each component by the inverse of the magnitude
                      v1.X * inverse,
                      v1.Y * inverse                      
                   )
                );
            }
        }

        /// <summary>
        /// Method for returning the normalized version of this vector.
        /// </summary>
        /// <returns></returns>
        public Vector2 Normalize()
        {
            return Normalize(this);
        }

        /// <summary>
        /// Method for rotating a vector over a number of degrees.
        /// </summary>
        /// <param name="degree"></param>
        /// <returns></returns>
        public Vector2 Rotate(double degree)
        {
            degree = DegreeToRadian(degree);
            double x = (this.X * Math.Cos(degree)) - (this.Y * Math.Sin(degree));
            double y = (this.X * Math.Sin(degree)) + (this.Y * Math.Cos(degree));            
            return new Vector2(x, y);
        }

        /// <summary>
        /// Addition operator.
        /// </summary>
        /// <param name="v1"></param>
        /// <param name="v2"></param>
        /// <returns></returns>
        public static Vector2 operator +(Vector2 v1, Vector2 v2)
        {
            return
            (
               new Vector2
               (
                  v1.X + v2.X,
                  v1.Y + v2.Y                  
               )
            );
        }

        /// <summary>
        /// Substraction operator.
        /// </summary>
        /// <param name="v1"></param>
        /// <param name="v2"></param>
        /// <returns></returns>
        public static Vector2 operator -(Vector2 v1, Vector2 v2)
        {
            return
            (
               new Vector2
               (
                   v1.X - v2.X,
                   v1.Y - v2.Y
               )
            );
        }

        /// <summary>
        /// Scalar multiplication operator.
        /// </summary>
        /// <param name="v1"></param>
        /// <param name="s2"></param>
        /// <returns></returns>
        public static Vector2 operator *(Vector2 v1, double s2)
        {
            return
            (
               new Vector2
               (
                  v1.X * s2,
                  v1.Y * s2
               )
            );
        }

        /// <summary>
        /// Scalar division operator.
        /// </summary>
        /// <param name="v1"></param>
        /// <param name="s2"></param>
        /// <returns></returns>
        public static Vector2 operator /(Vector2 v1, double s2)
        {
          return
          (
             new Vector2
             (
                v1.X / s2,
                v1.Y / s2
             )
          );
        }

        /// <summary>
        /// Static method for transforming an angle in degrees to an angle in radians.
        /// </summary>
        /// <param name="angle"></param>
        /// <returns></returns>
        public static double DegreeToRadian(double angle)
        {
            return Math.PI * angle / 180.0;
        }

    }
}
