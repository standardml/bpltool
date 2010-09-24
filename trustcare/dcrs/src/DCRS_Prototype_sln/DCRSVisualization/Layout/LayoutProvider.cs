using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ITU.DK.DCRS.Visualization.Layout
{
    /// <summary>
    /// Interface defining a layout provider.
    /// </summary>
    /// <typeparam name="ST">State type of the graph that the layoutprovider operates on.</typeparam>
    /// <typeparam name="LT">Label type of the graph that the layoutprovider operates on.</typeparam>
    interface LayoutProvider<ST, LT> where ST : IEquatable<ST>
    {        
        /// <summary>
        /// Executes the underlying algorithm of the layout provider.
        /// </summary>
        void Run();

        /// <summary>
        /// After executing returns the positions of the states in the graph as vectors.
        /// </summary>
        /// <returns></returns>
        Dictionary<ST, Vector2> GetNodePositions();
    }  
}
