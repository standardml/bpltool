using System;
using System.IO;
using System.Linq;
using System.Xml.Linq;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace ITU.DK.DCRS.CommonTypes.Serialization
{
    public class DCRSModelSerializer
    {



        public static string SerializeDCRSModel(DCRSModel model)
        {

            // actions elements.
            var actionsElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "actions");

            //foreach (KeyValuePair<short, string> keyValuePair in model.ActionList)
            //{
            //    var actionElement = new XElement("action",
            //                                     new XAttribute("id", keyValuePair.Key),
            //                                     new XAttribute("name", keyValuePair.Value),
            //                                     new XAttribute("included", string.Empty)
            //        );

            //    actionsElement.Add(actionElement);
            //}


            foreach (var actionElement in
                model.ActionList.Select(keyValuePair => new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "action", new XAttribute("id", keyValuePair.Key), new XAttribute("name", keyValuePair.Value), new XAttribute("included", string.Empty))))
            {
                actionsElement.Add(actionElement);
            }


            // constraint-set-collection element.
            var constraintsElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "constraint-set-collection",
                WriteConstrintSet(model.Conditions, "condition"),
                WriteConstrintSet(model.StrongConditions, "strongcondition"),
                WriteConstrintSet(model.MileStones, "milestone"),
                WriteConstrintSet(model.Responses, "response"),
                WriteConstrintSet(model.Includes, "include"),
                WriteConstrintSet(model.Excludes, "exclude")
                );



            var dcrsElement =
                new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs",
                             new XAttribute("xmlns", GlobalDeclarations.DCRS_NAMESPACE_URI),
                             new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-specification",
                                          new XAttribute("model-name", model.ModelName),
                                          actionsElement, constraintsElement
                                 ),
                             new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-runtime",
                                          new XAttribute("instance-id", string.Empty))

                    );

            var writer = new StringWriter();

            dcrsElement.Save(writer);

            writer.Flush();

            return writer.ToString();
        }



        public static DCRSModel DeSerializeDCRSModel(string dcrsXml)
        {

            if (string.IsNullOrEmpty(dcrsXml)) throw new ArgumentNullException("dcrsXml");

            try
            {
                var reader = new StringReader(dcrsXml);

                var rootElement = XElement.Load(reader);

                var specElement = rootElement.Element(GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-specification");

                var modelName = rootElement.Element(GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-specification").Attribute("model-name").Value;


                var actions = from ele in rootElement.Descendants(GlobalDeclarations.DCRS_NAMESPACE_URI + "action")
                              select ele;

                var actionList = actions.ToDictionary(action => short.Parse(action.Attribute("id").Value), action => action.Attribute("name").Value);


                var constraintSetElements = from ele in rootElement.Descendants(GlobalDeclarations.DCRS_NAMESPACE_URI + "constraint-set")
                                            select ele;


                var includes = GetContraintsArray(rootElement, "include");

                var excludes = GetContraintsArray(rootElement, "exclude");

                var responses = GetContraintsArray(rootElement, "response");

                var conditions = GetContraintsArray(rootElement, "condition");

                var strongconditions = GetContraintsArray(rootElement, "strongcondition");

                var milestones = GetContraintsArray(rootElement, "milestone");




                return new DCRSModel(modelName, actionList, includes, excludes, responses, conditions, strongconditions,
                                     milestones);


            }
            catch (Exception exception)
            {

                throw new ApplicationException(string.Format("Failed to de-serialize dcrs xml. Error Message:{0}",
                                                             exception.Message));
            }

            //string modelName 









            return null;
        }


        private static short[,] GetContraintsArray(XElement rootElement, string constraintType)
        {

            //var condElements = from ele in rootElement.Descendants(GlobalDeclarations.DCRS_NAMESPACE_URI + "constraint-set")
            //                   where ele.Attribute("type").Value == constraintType
            //                   select ele.Elements();

            var elements = from ele in rootElement.Descendants(GlobalDeclarations.DCRS_NAMESPACE_URI + "constarint")
                           where ele.Parent.Attribute("type").Value == constraintType
                           select ele;

            var constraintArray = new short[elements.Count(), 2];


            int index = 0;

            foreach (XElement element in elements)
            {

                constraintArray[index, 0] = short.Parse(element.Attribute("source").Value);

                constraintArray[index, 1] = short.Parse(element.Attribute("target").Value);

                index++;

            }

            return constraintArray;



        }


        private static XElement WriteConstrintSet(short[,] constraintset, string type)
        {
            // constraint-set-collection element.
            var constraintsetElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "constraint-set", new XAttribute("type", type));

            for (short index = 0; index < constraintset.GetLength(0); index++)
            {
                var constraintElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "constarint", new XAttribute("source", constraintset[index, 0]),
                                                     new XAttribute("target", constraintset[index, 1])
                    );

                constraintsetElement.Add(constraintElement);
            }


            return constraintsetElement;
        }





    }
}
