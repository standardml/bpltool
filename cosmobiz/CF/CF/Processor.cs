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
        }
    }
}
