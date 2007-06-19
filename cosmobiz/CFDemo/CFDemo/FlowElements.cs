using System;
using System.Collections;
using System.Text;

namespace CFDemo //(http://www.codeproject.com/csharp/iinterfaces_p1.asp)
{
    public class Element
    {
        private string name;
        private string type;
        private string owner;

        public Element(string name, string type, string owner)
        {
            this.name = name;
            this.type = type;
            this.owner = owner;
        }

        public Element()
        {
            Name = "name";
            Type = "type";
            Owner = "";
        }
        public string Name
        {
            get { return name; }
            set { name = value; }
        }
        public string Type
        {
            get { return type; }
            set { type = value; }
        }

        public string Owner
        {
            get { return owner; }
            set { owner = value; }
        }

    }






    public class ListedElements : IEnumerable
    {
        Element[] elements = new Element[1];
        int count = 0;

        public Element this[int index]
        {
            get
            {
                return (elements[index]);
            }
            set
            {
                elements[index] = value;
            }
        }

        internal int Count
        {
            get
            {
                return (count);
            }
        }

        public void Add(Element elem)
        {
            if (count >= elements.Length)
            {
                elements = (Element[])Resize(elements, elements.Length + 1);
            }
            elements[count++] = elem;
        }

        public static Array Resize(Array array, int newSize)
        {
            Type type = array.GetType().GetElementType();
            Array newArray = Array.CreateInstance(type, newSize);
            Array.Copy(array, 0, newArray, 0, Math.Min(array.Length, newSize));
            return newArray;
        }

        #region IEnumerable Members

        public IEnumerator GetEnumerator()
        {
            return (new ListedElementsEnumerator(this));
        }

        #endregion
    }





    public class ListedElementsEnumerator : IEnumerator
    {
        ListedElements listedElements;
        int index;

        public ListedElementsEnumerator(ListedElements listedElements)
        {
            this.listedElements = listedElements;
            Reset();
        }

        #region IEnumerator Members

        public object Current
        {
            get
            {
                return listedElements[index];
            }
        }

        public bool MoveNext()
        {
            if (++index >= listedElements.Count)
                return (false);
            else
                return (true);
        }

        public void Reset()
        {
            index = -1;
        }



        #endregion
    }
}
