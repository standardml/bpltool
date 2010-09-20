// Simple force based algorithm.
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;

namespace ITU.DK.DCRS.Visualization.Layout
{
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

     
        /*
         
 set up initial node velocities to (0,0)
 set up initial node positions randomly // make sure no 2 nodes are in exactly the same position
 loop
     total_kinetic_energy := 0 // running sum of total kinetic energy over all particles
     for each node
         net-force := (0, 0) // running sum of total force on this particular node
         
         for each other node
             net-force := net-force + Coulomb_repulsion( this_node, other_node )
         next node
         
         for each spring connected to this node
             net-force := net-force + Hooke_attraction( this_node, spring )
         next spring
         
         // without damping, it moves forever
         this_node.velocity := (this_node.velocity + timestep * net-force) * damping
         this_node.position := this_node.position + timestep * this_node.velocity
         total_kinetic_energy := total_kinetic_energy + this_node.mass * (this_node.velocity)^2
     next node
 until total_kinetic_energy is less than some small number  // the simulation has stopped moving         
                  
         */
    

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
    

        /*
              total_kinetic_energy := 0 // running sum of total kinetic energy over all particles
             for each node
                 net-force := (0, 0) // running sum of total force on this particular node
         
                 for each other node
                     net-force := net-force + Coulomb_repulsion( this_node, other_node )
                 next node
         
                 for each spring connected to this node
                     net-force := net-force + Hooke_attraction( this_node, spring )
                 next spring
         
                 // without damping, it moves forever
                 this_node.velocity := (this_node.velocity + timestep * net-force) * damping
                 this_node.position := this_node.position + timestep * this_node.velocity
                 total_kinetic_energy := total_kinetic_energy + this_node.mass * (this_node.velocity)^2
             next node

 
         */

        float timestep = 0f;
        float damping = 0.1f;
    
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

        // just implement vectors for the velocities? and positions for that matter...

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
                //double factor = Math.Round((50 - dis));
                //float repulsion = (float)(Math.Pow(2, factor) * -1);                                
                //double factor = Math.Round((200 - dis))/10;
                //float repulsion = (float)((factor * factor) * -1);                                
                //float repulsion = (float)(Math.Pow(2, factor) * -1);                                

                float adis = Math.Max(1, dis - 200);

                float repulsion = 20000f / (adis * adis);


                dif = dif.Normalize();
                change = dif * repulsion;
                result = result + change;
            }

            /*
            if (s.ToString() == "B")
            {
                Debug.Write("Repellor: ");
                Debug.Write(s2);
                Debug.Write("Netforce: ");
                Debug.Write(netforce);
                Debug.Write(" Difference: ");
                Debug.Write(dif);
                Debug.Write(" Distance: ");
                Debug.Write(dis);
                Debug.Write(" Change: ");
                Debug.Write(change);
                Debug.Write(" Result: ");
                Debug.WriteLine(result);
            }
            */
            

            /*
            if ((Math.Abs(Math.Abs(NodePositions[s].lhs) - Math.Abs(NodePositions[s2].lhs)) < 10) 
                && (Math.Abs(Math.Abs(NodePositions[s].lhs) - Math.Abs(NodePositions[s2].lhs)) > 0))
            {
                result.lhs += 1 / (NodePositions[s].lhs - NodePositions[s2].lhs);
            }
            if ((Math.Abs(Math.Abs(NodePositions[s].rhs) - Math.Abs(NodePositions[s2].rhs)) < 10)
                            && (Math.Abs(Math.Abs(NodePositions[s].rhs) - Math.Abs(NodePositions[s2].rhs)) > 0))
            {
                result.rhs += 1 / (NodePositions[s].rhs - NodePositions[s2].rhs);
            }*/
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
