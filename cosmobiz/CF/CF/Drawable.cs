using System;
using System.Collections.Generic;
using System.Text;

namespace CF
{
    public abstract class Drawable
    {
        public abstract void Draw();
        public abstract void AddChild(Drawable child);
    }
}
