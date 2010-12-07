using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Xml.Serialization;
using System.IO;
using ITU.DK.DCRS.CommonTypes.Serialization;


namespace ITU.DK.DCRS.CommonTypes.Process
{
    /// <summary>
    /// Class that makes it possible to store a placement/layout of nodes to a xml file.    
    /// </summary>
    /// <typeparam name="ST"></typeparam>
    public class DCRSProcessLayout
    {
        public int processID;
        public string role;
        public SerializableDictionary<short, Point> NodeLocations;

        public DCRSProcessLayout()
        {
            NodeLocations = new SerializableDictionary<short, Point>();
        }


        public void Add(short n, Point p)
        {
            NodeLocations.Add(n, p);
        }

        public void Remove(short n)
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

            SerializableDictionary<short, Point> t = new SerializableDictionary<short, Point>();

            foreach (var x in NodeLocations)
            {
                t.Add(x.Key, new Point((x.Value.X - minX) + 55 + 100, (x.Value.Y - minY) + 85 + 30));
            }
            
            NodeLocations = t;
        }


        public Boolean MoveNode(short id, Point newLocation)
        {
            NodeLocations[id] = newLocation;
            return true;
        }


        public int AlignX(int a, int x)
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


        public int AlignY(int a, int y)
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

        public void SerializeToXML(String path)
        {
            XmlSerializer serializer = new XmlSerializer(typeof(DCRSProcessLayout));
            TextWriter textWriter = new StreamWriter(path);
            serializer.Serialize(textWriter, this);
            textWriter.Close();
        }


        static public void SerializeToXML(DCRSProcessLayout p, String path)
        {
            XmlSerializer serializer = new XmlSerializer(typeof(DCRSProcessLayout));
            TextWriter textWriter = new StreamWriter(path);
            serializer.Serialize(textWriter, p);
            textWriter.Close();
        }

        static public DCRSProcessLayout DeserializeFromXML(String path)
        {
            XmlSerializer deserializer = new XmlSerializer(typeof(DCRSProcessLayout));
            TextReader textReader = new StreamReader(path);
            DCRSProcessLayout p;
            p = (DCRSProcessLayout)deserializer.Deserialize(textReader);
            textReader.Close();
            return p;
        }

        static public string Serialize(DCRSProcessLayout val)
        {
            XmlSerializer serializer = new XmlSerializer(typeof(DCRSProcessLayout));
            StringBuilder sb = new StringBuilder();
            StringWriter stringWriter = new StringWriter(sb);
            serializer.Serialize(stringWriter, val);
            return sb.ToString();
        }

        static public DCRSProcessLayout Deserialize(String val)
        {
            XmlSerializer deserializer = new XmlSerializer(typeof(DCRSProcessLayout));
            TextReader textReader = new StringReader(val);
            //serializer.Serialize            
            return (DCRSProcessLayout)deserializer.Deserialize(textReader);            
        }

    }
}
