using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;

namespace CF
{
    public abstract class Drawable
    {
        //For testing
        public abstract void setName(string name);
        public abstract string getName();
        //
        public abstract Size Size();
        public abstract Point Draw(MainWindow main, Point point);
        public abstract void AddChild(Drawable child);
        public abstract void AddParent(Drawable parent);
        public abstract Drawable GetParent();
        public abstract Size CollectSize();
    }
}
