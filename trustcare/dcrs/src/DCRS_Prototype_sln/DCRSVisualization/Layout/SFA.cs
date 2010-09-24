using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;

namespace ITU.DK.DCRS.Visualization.Layout
{
    /// <summary>
    /// Layoutprovider that implements a fairly basic force-based algorithm.
    /// </summary>
    /// <typeparam name="ST">State type</typeparam>
    /// <typeparam name="LT">Label type</typeparam>
    class SFALayoutProvider<ST, LT> : LayoutProvider<ST, LT> where ST : IEquatable<ST>
    {
        public Dictionary<ST, Vector2> NodePositions;
        public Dictionary<ST, Vector2> NodeVelocities;
        Graph<ST, LT> TargetGraph;
        public Set<ST> StabileNodes;

        public SFALayoutProvider(Graph<ST, LT> g)
        {
            NodeVelocities = new Dictionary<ST, Vector2>();
            NodePositions = new Dictionary<ST, Vector2>();
            StabileNodes = new Set<ST>();
            TargetGraph = g;            
        }


        public Dictionary<ST, Vector2> GetNodePositions()
        {
            Dictionary<ST, Vector2> result = new Dictionary<ST, Vector2>();
            foreach (var a in NodePositions)
                result.Add(a.Key, a.Value);
            return result;
        }

        /// <summary>
        /// Initializes the layout provider.
        /// </summary>
        public void initRun()
        {
            Random rand = new Random();

            NodeVelocities.Clear();
            NodePositions.Clear();
            foreach (ST s in TargetGraph.States.Difference(StabileNodes))
            {
                NodeVelocities.Add(s, new Vector2(0, 0));
                NodePositions.Add(s, new Vector2(rand.Next(100, 900), rand.Next(100, 900)));                
            }

            foreach (ST s in StabileNodes)
            {
                NodeVelocities.Add(s, new Vector2(0, 0));
                NodePositions.Add(s, new Vector2(500, 500));
            }
        }

        /// <summary>
        /// Executes the algorithm until it ends.
        /// </summary>
        public void Run()
        {
            initRun();

            float total_kinetic_energy = 100f;
            while (total_kinetic_energy > 10f)
            {
                total_kinetic_energy = RunOnce();
                Debug.WriteLine(total_kinetic_energy);
            }            
        }
    
        float timestep = 0f;
        float damping = 0.1f;

        /// <summary>
        /// Executes the algorithm for a single step.
        /// </summary>
        /// <returns></returns>    
        public float RunOnce()
        {
            float total_kinetic_energy = 0;
            timestep++;
            foreach (ST s in TargetGraph.States.Difference(StabileNodes))
            {
                Vector2 netforce = new Vector2(0, 0);
                foreach (ST s2 in TargetGraph.States)
                {
                    if (!s.Equals(s2)) netforce = Coulomb_repulsion(netforce, s, s2);
                }

                //springs as connected nodes?
                // makes sense...
                foreach (Edge<ST, LT> e in TargetGraph.OutgoingEdges[s])
                {
                    netforce = Hooke_attraction(netforce, s, e.d);
                }

                foreach (Edge<ST, LT> e in TargetGraph.IncomingEdges[s])
                {
                    netforce = Hooke_attraction(netforce, s, e.s);
                }


                 NodeVelocities[s] = (NodeVelocities[s] + (netforce)) * damping;
                 NodePositions[s] = NodePositions[s] + (NodeVelocities[s]);
                 total_kinetic_energy = total_kinetic_energy + (NodeVelocities[s].fLength() * NodeVelocities[s].fLength());
            }

            return total_kinetic_energy;
        }


        /// <summary>
        /// Calculates the attraction between two nodes because of a connecting edge.
        /// </summary>
        /// <param name="netforce"></param>
        /// <param name="s"></param>
        /// <param name="s2"></param>
        /// <returns></returns>
        private Vector2 Hooke_attraction(Vector2 netforce, ST s, ST s2)
        {
            //throw new NotImplementedException();
            Vector2 result = new Vector2(netforce.X, netforce.Y);
            Vector2 dif = NodePositions[s] - NodePositions[s2];
            float dis = dif.fLength();

            if (dis == 0)
            {
                Vector2 change = new Vector2(0f, 0f);
                result = result + change;
            }
            else if (dis > 100)
            {
                float attraction = (dis-100) * -1f;
                attraction = attraction * 0.25f; // somehow make this dynamic...
                dif = dif.Normalize();
                Vector2 change = dif * attraction;
                result = result + change;
            }
            return result;
        }

        /// <summary>
        /// Calculates the repulsion between two nodes.
        /// </summary>
        /// <param name="netforce"></param>
        /// <param name="s"></param>
        /// <param name="s2"></param>
        /// <returns></returns>
        private Vector2 Coulomb_repulsion(Vector2 netforce, ST s, ST s2)
        {
            Vector2 result = new Vector2(netforce.X, netforce.Y);

            Vector2 dif = NodePositions[s] - NodePositions[s2];
            float dis = dif.fLength();

            Vector2 change = new Vector2();
            if (dis == 0)
            {
                //change = new Vector2(10f, 10f);
                //result = result + change;
            }
            else if (dis < 300)
            {
                float adis = Math.Max(1, dis - 200);

                float repulsion = 20000f / (adis * adis);

                dif = dif.Normalize();
                change = dif * repulsion;
                result = result + change;
            }
            return result;
        }
    }


    

    public class Pair<LT, RT> where LT : IEquatable<LT> where RT : IEquatable<RT>
    {
        public LT lhs;
        public RT rhs;

        public Pair(LT l, RT r)
        {
            lhs = l;
            rhs = r;
        }
    }  
}
