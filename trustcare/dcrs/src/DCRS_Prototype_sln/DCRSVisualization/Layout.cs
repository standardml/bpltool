﻿using System;
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
    /// </summary>
    /// <typeparam name="ST"></typeparam>
    public class Layout<ST> where ST : IEquatable<ST>
    {
        private static string placementPath;
        public int processID;
        public int instanceID;
        public string role;
        public SerializableDictionary<ST, Point> NodeLocations;

        public Layout()
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


        public static Layout<ST> FromLayoutProvider(LayoutProvider<ST, bool> layoutProvider)
        {
            Layout<ST> result = new Layout<ST>();

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
            XmlSerializer serializer = new XmlSerializer(typeof(Layout<ST>));
            TextWriter textWriter = new StreamWriter(getFilePath(processID,instanceID));
            serializer.Serialize(textWriter, this);
            textWriter.Close();
        }


        static public void SerializeToXML(Layout<ST> p)
        {
            XmlSerializer serializer = new XmlSerializer(typeof(Layout<ST>));
            TextWriter textWriter = new StreamWriter(getFilePath(p.processID,p.instanceID));
            serializer.Serialize(textWriter, p);
            textWriter.Close();
        }

        static public Layout<ST> DeserializeFromXML(int processId, int instancId)
        {
            XmlSerializer deserializer = new XmlSerializer(typeof(Layout<ST>));
            TextReader textReader = new StreamReader(getFilePath(processId,instancId));
            Layout<ST> p;
            p = (Layout<ST>)deserializer.Deserialize(textReader);
            textReader.Close();

            p.processID = processId;
            p.instanceID = instancId;

            return p;
        }

        static public Layout<ST> DeserializeFromXML(int processId)
        {
            XmlSerializer deserializer = new XmlSerializer(typeof(Layout<ST>));
            TextReader textReader = new StreamReader(getFilePath(processId,0));
            Layout<ST> p;
            p = (Layout<ST>)deserializer.Deserialize(textReader);
            textReader.Close();

            p.processID = processId;

            return p;
        }

        public static string getFilePath(int processId, int instanceId)
        {
            if (placementPath == null)
                initPlacementDir();
            String fileName = "placement#" + processId.ToString();
            if (instanceId > 0)
                fileName = fileName + "#" + instanceId.ToString();
            fileName = fileName + "#.xml";
            return System.IO.Path.Combine(placementPath, fileName);
        }

        // create the placement Directory if necessary. Init placementPath.
        public static void initPlacementDir()
        {
            Uri appUri = new Uri(System.IO.Path.GetDirectoryName(
                                     System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase));
            String appPath = System.IO.Path.GetFullPath(appUri.LocalPath); // absolute path gives a path containing %20 for spaces, which windows(XP) can nto handle            
#if DEBUG
            // so that they can be stored and editted in the solution
            String placementPath;
            if (appPath.Contains("Debug")) // check is not really correct, but easy... at some point see if there isn't a way of checking if its being used by a webapplication.
                placementPath = System.IO.Path.Combine(appPath, @"..\..\placements"); // In the case of a normal application
            else
                placementPath = System.IO.Path.Combine(appPath, @"..\placements"); // In the case of a web application
#else
            String placementPath = System.IO.Path.Combine(appPath, @".\placements"); // so that they can be deployed with the release version
#endif
            if (!System.IO.Directory.Exists(placementPath))
                System.IO.Directory.CreateDirectory(placementPath);
            Layout<ST>.placementPath = placementPath;
        }

    }
}