using System;
using System.Collections.Generic;
using System.Text;

namespace CFDemo
{
    enum Side { left = 1, right = 2, center = 3 };
    public abstract class DrawBase
    {
        
        protected abstract void Draw();
    }
}
