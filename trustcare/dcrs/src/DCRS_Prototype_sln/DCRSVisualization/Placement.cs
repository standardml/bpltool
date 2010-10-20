using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Xml.Serialization;
using System.IO;
using ITU.DK.DCRS.Visualization.Layout;

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
        public SerializableDictionary<ST, Point> NodeLocations;

        public Placement()
        {
            NodeLocations = new SerializableDictionary<ST, Point>();
        }


        public void Add(ST n, Point p)
        {
            NodeLocations.Add(n, p);
        }

        public void Remove(ST n)
        {
            NodeLocations.Remove(n);
        }

        public void ShiftTowardsTopLeft()
        {
            int minX = 9999999;
            int minY = 9999999;
            foreach (var x in NodeLocations)
            {
                minX = Math.Min(x.Value.X, minX);
                minY = Math.Min(x.Value.Y, minY);
            }

            SerializableDictionary<ST, Point> t = new SerializableDictionary<ST, Point>();

            foreach (var x in NodeLocations)
            {
                t.Add(x.Key, new Point((x.Value.X - minX) + 55 + 100, (x.Value.Y - minY) + 85 + 30));
            }
            
            NodeLocations = t;
        }


        public static Placement<ST> FromLayoutProvider(LayoutProvider<ST, bool> layoutProvider)
        {
            Placement<ST> result = new Placement<ST>();

            layoutProvider.Run();

            foreach (var x in layoutProvider.GetNodePositions())
            {
                result.NodeLocations.Add(x.Key, new Point((int)Math.Round(x.Value.X), (int)Math.Round(x.Value.Y)));
            }

            return result;
        }

        public Boolean MoveNode(ST id, Point newLocation)
        {
            NodeLocations[id] = newLocation;
            return true;
        }


        public int AlignX(ST a, int x)
        {
            int minDif=10;
            int alignedX = x;
            foreach (var v in NodeLocations)
            {
                if (!v.Key.Equals(a))
                    if (Math.Abs(v.Value.X - x) < minDif)
                        alignedX = v.Value.X;
            }
            return alignedX;
        }


        public int AlignY(ST a, int y)
        {
            int minDif = 10;
            int alignedY = y;
            foreach (var v in NodeLocations)
            {
                if (!v.Key.Equals(a))
                    if (Math.Abs(v.Value.Y - y) < minDif)
                        alignedY = v.Value.Y;
            }
            return alignedY;
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

            p.processID = processId;
            p.instanceID = instancId;

            return p;
        }

        static public Placement<ST> DeserializeFromXML(int processId)
        {
            XmlSerializer deserializer = new XmlSerializer(typeof(Placement<ST>));
            TextReader textReader = new StreamReader(@"C:\PhDWork\placements\placement#" + processId.ToString() + "#.xml");
            Placement<ST> p;
            p = (Placement<ST>)deserializer.Deserialize(textReader);
            textReader.Close();

            p.processID = processId;

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
