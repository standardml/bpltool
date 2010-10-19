using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;


namespace DCRSGraphicalEditor
{
    class DoubleBufferPanel : Panel
    {
        public DoubleBufferPanel()
        {

            // Set the value of the double-buffering style bits to true.

            this.DoubleBuffered = true;

            /*
            this.SetStyle(ControlStyles.AllPaintingInWmPaint |

            ControlStyles.UserPaint |

            ControlStyles.OptimizedDoubleBuffer, true);

            UpdateStyles();
             */
        }

            

    }
}
