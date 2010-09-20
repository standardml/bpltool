using System;
using System.IO;
using System.Xml.Linq;
using ITU.DK.DCRS.CommonTypes.Exceptions;
using ITU.DK.DCRS.CommonTypes.Serialization;

namespace ITU.DK.DCRS.CommonTypes.Process
{
    public class DCRSProcess
    {
        public DCRSSpecification Specification;

        public DCRSRuntime Runtime;

        #region Static Functions.

        public static DCRSProcess Load(string filepath)
        {
            try
            {

                var reader = new StreamReader(filepath);

                var dcrsXml = reader.ReadToEnd();

                reader.Close();

                return new DCRSProcess
                                  {
                                      Specification =
                                          DCRSSpecificationSerializer.DeserializeDCRSSpecificationXml(dcrsXml),
                                      Runtime = DCRSRuntimeSerializer.DeserializeDCRSRuntimeXml(dcrsXml)
                                  };
            }
            catch (Exception exception)
            {

                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to load DCRSProcess from file: {0}. Error message: {1}. ", filepath,
                        DCRSWorkflowException.GetCombinedMessage(exception)));

            }

        }

        public static void Save(DCRSProcess process, string filepath)
        {
            try
            {

                var processElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-process",
                                                       DCRSSpecificationSerializer.SerializeDCRSSpecificationToXElement(
                                                           process.Specification),
                                                       DCRSRuntimeSerializer.SerializeDCRSRuntimeToXElement(
                                                           process.Runtime));

                processElement.Save(filepath);


            }
            catch (Exception exception)
            {

                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to save to DCRSProcess to a file: {0}. Error message: {1}. ", filepath,
                        DCRSWorkflowException.GetCombinedMessage(exception)));

            }


        }

        public static string Serialize(DCRSProcess process)
        {

            try
            {
                var processElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-process",
                                                       DCRSSpecificationSerializer.SerializeDCRSSpecificationToXElement(
                                                           process.Specification),
                                                       DCRSRuntimeSerializer.SerializeDCRSRuntimeToXElement(
                                                           process.Runtime));
                var stringWriter = new StringWriter();

                processElement.Save(stringWriter);

                var processXml = stringWriter.ToString();

                stringWriter.Close();

                return processXml;

            }
            catch (Exception exception)
            {

                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to serialize DCRSProcess. Error message: {0}. ",
                        DCRSWorkflowException.GetCombinedMessage(exception)));
            }
        }


        public static DCRSProcess Deserialize(string processXml)
        {

            try
            {
                return new DCRSProcess()
                           {
                               Specification = DCRSSpecificationSerializer.DeserializeDCRSSpecificationXml(processXml),
                               Runtime = DCRSRuntimeSerializer.DeserializeDCRSRuntimeXml(processXml)
                           };

            }
            catch (Exception exception)
            {

                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to deserialize processXml to DCRSProcess. Error message: {0}. ",
                        DCRSWorkflowException.GetCombinedMessage(exception)));
            }

        }

        #endregion



    }
}
