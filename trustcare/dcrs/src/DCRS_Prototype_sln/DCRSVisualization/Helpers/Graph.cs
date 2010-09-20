using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ITU.DK.DCRS.Visualization
{  
    public class Edge<ST, LT> where ST : IEquatable<ST>
    {
        public ST s;
        public ST d;
        public LT label;
        public Edge(ST i, ST j, LT l)
        {
            s = i;
            d = j;
            label = l;
        }
    }

    public class Graph<ST, LT> where ST : IEquatable<ST>
    {
        public Set<ST> States;
        public Set<Edge<ST, LT>> Edges;
        //Dictionary<int, Object> StateLabels;
        //Dictionary<int, Object> EdgeLabels;
        //Dictionary<Object, int> StateLabelsReverse;
        //Dictionary<Object, int> EdgeLabelsReverse;
        public Dictionary<ST, Set<Edge<ST, LT>>> OutgoingEdges;
        public Dictionary<ST, Set<Edge<ST, LT>>> IncomingEdges;

        public Graph()
        {
            States = new Set<ST>();
            Edges = new Set<Edge<ST, LT>>();
            //StateLabels = new Dictionary<int, object>();
            //EdgeLabels = new Dictionary<int, object>();
            //StateLabelsReverse = new Dictionary<object, int>();
            //EdgeLabelsReverse = new Dictionary<object, int>();
            OutgoingEdges = new Dictionary<ST, Set<Edge<ST, LT>>>();
            IncomingEdges = new Dictionary<ST, Set<Edge<ST, LT>>>();
        }

        public Graph<ST, LT> AddState(ST o)
        {
            //StateLabels.Add(States.Count, o);
            //StateLabelsReverse.Add(o, States.Count);
            States.Add(o);

            if (!OutgoingEdges.ContainsKey(o)) OutgoingEdges.Add(o, new Set<Edge<ST, LT>>());
            if (!IncomingEdges.ContainsKey(o)) IncomingEdges.Add(o, new Set<Edge<ST, LT>>());

            return this;
        }


        public Graph<ST, LT> AddEdge(ST s, ST d, LT e)
        {
            Edge<ST, LT> ne = new Edge<ST, LT>(s, d, e);
            Edges.Add(ne);

            if (!OutgoingEdges.ContainsKey(s)) OutgoingEdges.Add(s, new Set<Edge<ST, LT>>());
            OutgoingEdges[s].Add(ne);

            if (!IncomingEdges.ContainsKey(d)) IncomingEdges.Add(d, new Set<Edge<ST, LT>>());
            IncomingEdges[d].Add(ne);

            return this;
        }

        public Set<Set<ST>> GetStronglyConnectedComponents()
        {
            Dictionary<ST, int> nodeIndex = new Dictionary<ST, int>();
            Dictionary<ST, int> nodeLowlink = new Dictionary<ST, int>();
            Set<Set<ST>> SCCs = new Set<Set<ST>>();

            int index = 0;

            Stack<ST> S = new Stack<ST>();
            foreach (ST v in this.States)
            {
                if (!nodeIndex.ContainsKey(v))

                    trajan(v, ref index, ref nodeIndex, ref nodeLowlink, ref S, ref SCCs);
            }

            return SCCs;
        }

        internal void trajan(ST v, ref int index, ref Dictionary<ST, int> nodeIndex, ref Dictionary<ST, int> nodeLowLink, ref Stack<ST> S, ref Set<Set<ST>> SCCs)
        {
            nodeIndex.Add(v, index);
            nodeLowLink.Add(v, index);
            index++;
            S.Push(v);
            foreach (Edge<ST, LT> e in OutgoingEdges[v])
            {
                ST vprime = e.d;
                if ((!nodeIndex.ContainsKey(vprime)) || (S.Contains(vprime)))
                {
                    if (!nodeIndex.ContainsKey(vprime)) trajan(vprime, ref index, ref nodeIndex, ref nodeLowLink, ref S, ref SCCs);
                    if (nodeLowLink[vprime] < nodeLowLink[v])
                    {
                        nodeLowLink.Remove(v);
                        nodeLowLink.Add(v, nodeLowLink[vprime]);
                    }
                }
            }
            if (nodeLowLink[v] == nodeIndex[v])
            {
                Set<ST> newSSC = new Set<ST>();
                ST vprime;
                //Debug.WriteLine("SCC:");
                do
                {
                    vprime = S.Pop();
                    //Debug.WriteLine(vprime.Name.ToString());
                    newSSC.Add(vprime);
                }
                while (!vprime.Equals(v));


                //if ((newSSC.Count > 1) || v.Incoming.Contains(v))
                //SCCs.Add(newSSC);

                bool selfloop = false;
                foreach (Edge<ST, LT> e in OutgoingEdges[v])
                    if (e.d.Equals(v)) selfloop = true;

                if ((newSSC.Count > 1) || selfloop)
                    SCCs.Add(newSSC);

            }
        }


        public void RemoveDeadStates(Set<ST> start)
        {
            Queue<ST> fringe = new Queue<ST>();
            Set<ST> closed_list = new Set<ST>();

            foreach (ST e in start)
            {
                closed_list.Add(e);
                fringe.Enqueue(e);
            }

            while (fringe.Count > 0)
            {
                ST e = fringe.Dequeue();
                foreach (Edge<ST, LT> ed in OutgoingEdges[e])
                {
                    if (!closed_list.Contains(ed.d))
                    {
                        fringe.Enqueue(ed.d);
                        closed_list.Add(ed.d);
                    }
                }
            }

            Set<ST> dead = States.Difference(closed_list);
            foreach (ST e in dead)
            {
                foreach (Edge<ST, LT> ed in OutgoingEdges[e])
                    Edges.Remove(ed);
                OutgoingEdges.Remove(e);
                States.Remove(e);
            }

        }


        public void BackwardRemoveDeadStates(Set<ST> start)
        {
            Queue<ST> fringe = new Queue<ST>();
            Set<ST> closed_list = new Set<ST>();

            foreach (ST e in start)
            {
                closed_list.Add(e);
                fringe.Enqueue(e);
            }

            while (fringe.Count > 0)
            {
                ST e = fringe.Dequeue();
                foreach (Edge<ST, LT> ed in IncomingEdges[e])
                {
                    if (!closed_list.Contains(ed.s))
                    {
                        fringe.Enqueue(ed.s);
                        closed_list.Add(ed.s);
                    }
                }
            }

            Set<ST> dead = States.Difference(closed_list);
            foreach (ST e in dead)
            {
                foreach (Edge<ST, LT> ed in IncomingEdges[e])
                {
                    Edges.Remove(ed);
                    if (OutgoingEdges.ContainsKey(ed.s))
                        OutgoingEdges[ed.s].Remove(ed);
                }
                foreach (Edge<ST, LT> ed in OutgoingEdges[e])
                    Edges.Remove(ed);

                OutgoingEdges.Remove(e);
                IncomingEdges.Remove(e);
                States.Remove(e);
            }

        }

    }
}
