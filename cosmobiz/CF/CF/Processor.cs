using System;
using System.Collections.Generic;
using System.Text;

namespace CF
{
    public class Processor
    {
        private ListedElements elements;
        /*
        private List<object> objects;

        public List<object> Objects
        {
            get { return objects; }
        }
        */
        public Processor(ListedElements elements)
        {
            this.elements = elements;
            //objects = new List<object>();
        }

        public void ProcessElements(int j, IDrawable obj)//, Drawable obj)
        {
            //for (int i = j; i < elements.Count; i++)
            //foreach (Element elem in elements)
            {
                switch (elements[j].Type)
                {
                    case "sequence":
                        {
                            //opret ny seq
                            //kald rekursiv med seq
                            Sequence seq = new Sequence();
                            objects.Add(seq);
                            ProcessElements(j + 1, seq); //rekursion, så parent kender children
                        }
                        break;
                    case "endsequence":
                        {
                            //break og return til parent
                            break;
                        }
                        break;
                    case "activity":
                        {

                            //opret ny act
                            //add activity til ejerens list over subObj
                        }
                        break;
                    case "Flow":
                        {
                            //Opret nyt flow
                            //kald rekursivt med flow
                        }
                        break;
                    case "endFlow":
                        {
                            //break og return til paret
                        }
                        break;

                    default:
                        break;
                }
            }
        }
    }
}
