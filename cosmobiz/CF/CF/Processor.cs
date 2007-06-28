using System;
using System.Collections.Generic;
using System.Text;

namespace CF
{
    class Processor
    {
        ListedElements elements;
        public Processor(ListedElements elements)
        {
            this.elements = elements;
        }

        public void ProcessElements()
        {
            foreach (Element elem in elements)
            {
                switch (elem.Type)
                {
                    case "sequence":
                        { }
                    case "endsequence":
                        { }
                    case "activity":
                        { }
                    case "Flow":
                        { }
                    case "endFlow":
                        { }

                    default:
                        break;
                }
            }
        }
    }
}
