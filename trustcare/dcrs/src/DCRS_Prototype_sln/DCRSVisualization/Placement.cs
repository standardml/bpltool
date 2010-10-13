using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Xml.Serialization;
using System.IO;

namespace ITU.DK.DCRS.Visualization
{
    public class Placement<ST> where ST : IEquatable<ST>
    {
        public int processID;
        public int instanceID;
        public List<ST> Keys;
        public List<Point>Values;

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
