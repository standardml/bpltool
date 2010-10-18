using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Xml.Serialization;
using System.IO;

namespace ITU.DK.DCRS.Visualization
{
    /// <summary>
    /// Class that makes it possible to store a placement/layout of nodes to a xml file.
    /// The clumsy storage mechanism is there to both enable xml serialization and still allow generic types. Dictionaries are not xml serializable regretfully.
    /// Would be good to improve on this in the future, for example by making a serializable dictionary, or making pairs of keys and values and storing those into a list.
    /// </summary>
    /// <typeparam name="ST"></typeparam>
    public class Placement<ST> where ST : IEquatable<ST>
    {
        public int processID;
        public int instanceID;
        public List<ST> Keys;       // Keys that represent the nodes of a graph.
        public List<Point>Values;   // The locations of those nodes.
                                    // Keys and Values should always remain in the same order.

        public Placement()
        {
            Keys = new List<ST>();
            Values = new List<Point>();
        }


        public void Add(ST n, Point p)
        {
            Keys.Add(n);
            Values.Add(p);
        }


        public void SerializeToXML()
        {
            XmlSerializer serializer = new XmlSerializer(typeof(Placement<ST>));
            TextWriter textWriter;
            if ((instanceID) > 0)
                textWriter = new StreamWriter(@"C:\PhDWork\placements\placement#" + processID.ToString() + "#" + instanceID.ToString() + "#.xml");
            else
                textWriter = new StreamWriter(@"C:\PhDWork\placements\placement#" + processID.ToString() + "#.xml");
            serializer.Serialize(textWriter, this);
            textWriter.Close();
        }


        static public void SerializeToXML(Placement<ST> p)
        {
            XmlSerializer serializer = new XmlSerializer(typeof(Placement<ST>));
            TextWriter textWriter = new StreamWriter(@"C:\PhDWork\placements\placement#" + p.processID.ToString() + "#" + p.instanceID.ToString() + "#.xml");
            serializer.Serialize(textWriter, p);
            textWriter.Close();
        }

        static public Placement<ST> DeserializeFromXML(int processId, int instancId)
        {
            XmlSerializer deserializer = new XmlSerializer(typeof(Placement<ST>));
            TextReader textReader = new StreamReader(@"C:\PhDWork\placements\placement#" + processId.ToString() + "#" + instancId.ToString() + "#.xml");
            Placement<ST> p;
            p = (Placement<ST>)deserializer.Deserialize(textReader);
            textReader.Close();

            return p;
        }

        static public Placement<ST> DeserializeFromXML(int processId)
        {
            XmlSerializer deserializer = new XmlSerializer(typeof(Placement<ST>));
            TextReader textReader = new StreamReader(@"C:\PhDWork\placements\placement#" + processId.ToString() + "#.xml");
            Placement<ST> p;
            p = (Placement<ST>)deserializer.Deserialize(textReader);
            textReader.Close();

            return p;
        }




        static public void SerializeToXMLOld(Placement<ST> p)
        {
            XmlSerializer serializer = new XmlSerializer(typeof(Placement<ST>));
            TextWriter textWriter = new StreamWriter(@"C:\PhDWork\placement.xml");
            serializer.Serialize(textWriter, p);
            textWriter.Close();
        }


        static public Placement<ST> DeserializeFromXMLOld()
        {
            XmlSerializer deserializer = new XmlSerializer(typeof(Placement<ST>));
            TextReader textReader = new StreamReader(@"C:\PhDWork\placement.xml");
            Placement<ST> p;
            p = (Placement<ST>)deserializer.Deserialize(textReader);
            textReader.Close();

            return p;
        }
    }
}
