using System;
using System.Collections.Generic;
using System.Text;

namespace CF
{
    public abstract class Drawable
    {
        public abstract System.Drawing.Point Draw(MainWindow main, System.Drawing.Point point);
        public abstract void AddChild(Drawable child);
        public abstract int CollectWidths();
    }
}
