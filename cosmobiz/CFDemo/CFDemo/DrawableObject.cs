using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;

namespace CFDemo
{
    public abstract class DrawableObject
    {
        protected enum Side { left, right, center };
        public bool draw = true;

        public abstract Point Draw(Point point);

    }
}