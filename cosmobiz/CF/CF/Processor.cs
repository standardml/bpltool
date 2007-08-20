using System;
using System.Collections.Generic;
using System.Text;

namespace CF
{
    public class Processor
    {
        private ListedElements elements;

        private Drawable startSequence;
        private Drawable currentNode;

        public Drawable StartSequence
        {
            get { return startSequence; }
        }

        public Processor(ListedElements elements)
        {
            this.elements = elements;
        }

        public void ProcessElements(int j)
        {
            if (j < elements.Count)
            {
                switch (elements[j].Type)       //Attributes from XML needs to be handled
                {
                    case "sequence":
                        {
                            Sequence seq = new Sequence();
                            if (currentNode != null)
                            {
                                seq.AddParent(currentNode);
                                currentNode.AddChild(seq);
                            }
                            else
                            {
                                startSequence = seq;
                            }
                            currentNode = seq;
                        }
                        break;
                    case "endsequence":
                        {
                            if (currentNode.GetParent() != null)
                            {
                                currentNode = currentNode.GetParent();
                            }
                        }
                        break;
                    case "activity":
                        {
                            Drawable act = new Activity();
                            act.AddParent(currentNode);
                            currentNode.AddChild(act);
                        }
                        break;
                    case "Flow":
                        {
                            Flow flow = new Flow();
                            flow.AddParent(currentNode);
                            currentNode.AddChild(flow);
                            currentNode = flow;
                        }
                        break;
                    case "endFlow":
                        {
                            currentNode = currentNode.GetParent();
                        }
                        break;

                    default:
                        break;
                }
                ProcessElements(j + 1);
            }
            //int t = startSequence.CollectWidths();
        }
    }
}





















/*
using System;
using System.Collections.Generic;
using System.Text;

namespace CF
{
    public class Processor
    {
        private ListedElements elements;

        private Drawable startSequence;
        private Sequence currentSequence;
        private Flow currentFlow;

        //private Drawable startSequence;
        public Drawable StartSequence
        {
            get { return startSequence; }
        }



        private List<Drawable> drawableObjects;

        public List<Drawable> DrawableObjects
        {
            get { return drawableObjects; }
            //set { drawableObjects = value; }
        }

        public Processor(ListedElements elements)
        {
            this.elements = elements;
            drawableObjects = new List<Drawable>();
        }

        public void ProcessElements(int j)
        {
            if (j < elements.Count)
            {
                switch (elements[j].Type)       //Attributes from XML needs to be handled
                {
                    case "sequence":
                        {
                            Sequence seq = new Sequence();

                            if (startSequence == null)
                            {
                                startSequence = seq;
                            }
                            if (currentSequence != null)
                            {
                                seq.AddParent(currentSequence);
                            }
                            if (currentFlow != null)
                            {
                                currentFlow.AddChild(seq);
                            }
                            currentSequence = seq;
                            drawableObjects.Add(seq);
                        }
                        break;
                    case "endsequence":
                        {
                            if (currentSequence.Parent != null)
                            {
                                currentSequence = currentSequence.Parent;
                            }
                        }
                        break;
                    case "activity":
                        {
                            Drawable act = new Activity();
                            currentSequence.AddChild(act);
                            drawableObjects.Add(act);
                        }
                        break;
                    case "Flow":
                        {
                            Flow flow = new Flow();

                            currentSequence.AddChild(flow);
                            currentFlow = flow;
                            drawableObjects.Add(flow);
                        }
                        break;
                    case "endFlow":
                        {
                            currentFlow = null;
                        }
                        break;

                    default:
                        break;
                }
                ProcessElements(j + 1);
            }
            int t = startSequence.CollectWidths();
        }
    }
}

*/