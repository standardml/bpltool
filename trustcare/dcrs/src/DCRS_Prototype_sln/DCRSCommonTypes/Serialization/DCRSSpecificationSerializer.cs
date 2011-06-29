using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml.Linq;
using ITU.DK.DCRS.CommonTypes.Exceptions;
using ITU.DK.DCRS.CommonTypes.Process;

namespace ITU.DK.DCRS.CommonTypes.Serialization
{
    public class DCRSSpecificationSerializer
    {

        #region 1. Serialization Methods.

        public static XElement SerializeDCRSSpecificationToXElement(DCRSSpecification specification)
        {
            try
            {

                #region 1. Resources.

                //roles element 
                var rolesElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "roles");

                foreach (var roleElement in specification.Roles.Select(role =>
                    new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "role", role)))
                {
                    rolesElement.Add(roleElement);
                }


                //principals element 
                var principalsElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "principals");

                foreach (var principalElement in specification.Principals.Select(principal =>
                    new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "principal", principal)))
                {
                    principalsElement.Add(principalElement);
                }


                // actions element 
                var actionsElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "actions");

                foreach (var actionElement in specification.ActionList.Select(actionKeyValPair =>
                    new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "action", new XAttribute("action-id", actionKeyValPair.Key), actionKeyValPair.Value)))
                {
                    actionsElement.Add(actionElement);
                }


                // resources elements.
                var resourcesElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "resources", rolesElement,
                                                    principalsElement, actionsElement);

                #endregion

                #region 2. Access Controls.

                #region Alternative coding.

                //foreach (var keyValuePair in specification.RolesToPrincipalsDictionary)
                //{
                //    var rolePrinciaplEle = new XElement(
                //        GlobalDeclarations.DCRS_NAMESPACE_URI + "role-principal-assignment",
                //        new XAttribute("rolename", keyValuePair.Key));

                //    foreach (var principalElement in keyValuePair.Value.Select(principal =>
                //    new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "principal", principal)))
                //    {
                //        rolePrinciaplEle.Add(principalElement);
                //    }

                //    rolesPrincipalsElement.Add(rolePrinciaplEle);
                //}


                //var keyvalPair1 = new KeyValuePair<string, List<string>>();

                //// Explore how to get the keyvalPair declared in select 
                //foreach (var rolePrincipalAssignEle in specification.RolesToPrincipalsDictionary.Select(keyvalPair =>
                //{
                //    keyvalPair1 = keyvalPair;
                //    return new XElement
                //        (GlobalDeclarations.DCRS_NAMESPACE_URI + "role-principal-assignment",
                //            new XAttribute("rolename", keyvalPair.Key));
                //}))
                //{

                //    foreach (var principalElement in keyvalPair1.Value.Select(principal =>
                //        new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "principal", principal)))
                //    {
                //        rolePrincipalAssignEle.Add(principalElement);
                //    }

                //    rolesPrincipalsElement.Add(rolePrincipalAssignEle);


                //}


                #endregion

                // role-principal-assignments element 
                var rolesPrincipalsElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "role-principal-assignments");

                // foreach in foreach to iterate through the main collection for roles and through each role,
                // foreach to iterate through principals list. 
                // Dictionary<string, List<string>> RolesToPrincipalsDictionary

                foreach (var rolePrincipalAssignEle in specification.RolesToPrincipalsDictionary.Select(keyvalPair =>
                {
                    var rolePrincipalAssignEle = new XElement
                        (GlobalDeclarations.DCRS_NAMESPACE_URI + "role-principal-assignment",
                         new XAttribute("role-name", keyvalPair.Key));

                    foreach (var principalElement in keyvalPair.Value.Select(principal =>
                         new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "principal", principal)))
                    {
                        rolePrincipalAssignEle.Add(principalElement);
                    }
                    return rolePrincipalAssignEle;
                }))
                {
                    rolesPrincipalsElement.Add(rolePrincipalAssignEle);
                }


                // action-role-assignments element 
                var actionsrolesElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "action-role-assignments");


                // foreach in foreach to iterate through the main collection for actions and through each action,
                // foreach to iterate through roles list. 
                // Dictionary<short, List<string>> ActionsToRolesDictionary

                foreach (var actionRoleAssignmentEle in specification.ActionsToRolesDictionary.Select(keyvalPair =>
                {
                    var actionRoleAssignmentEle = new XElement
                        (GlobalDeclarations.DCRS_NAMESPACE_URI + "action-role-assignment",
                         new XAttribute("action-id", keyvalPair.Key));

                    foreach (var roleElement in keyvalPair.Value.Select(role =>
                         new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "role", role)))
                    {
                        actionRoleAssignmentEle.Add(roleElement);
                    }
                    return actionRoleAssignmentEle;
                }))
                {
                    actionsrolesElement.Add(actionRoleAssignmentEle);
                }

                var accessControlsElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "access-controls",
                                                         rolesPrincipalsElement, actionsrolesElement);

                #endregion

                #region 3. Constraint-sets.


                // constraint-set-collection element.
                var constraintSetsElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "constraint-set-collection",
                    WriteConstrintSet(specification.Conditions, "condition"),
                    WriteConstrintSet(specification.StrongConditions, "strongcondition"),
                    WriteConstrintSet(specification.Responses, "response"),
                    WriteConstrintSet(specification.Includes, "include"),
                    WriteConstrintSet(specification.Excludes, "exclude"),
                    WriteConstrintSet(specification.Milestones, "milestone")
                    );

                #endregion



                #region 4. proerty-specification

                // proerty-specification 
                //var proertySpecification = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "proerty-specification");

                //runtime-monitoring-properties
                var runtimeMonitoringProperties = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "runtime-monitoring-properties");






                #endregion




                var specificationElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-specification",
                                                        new XAttribute("process-id", specification.ProcessId),
                                                        new XAttribute("model-name", specification.ModelName),
                                                        resourcesElement, accessControlsElement, constraintSetsElement);
                return specificationElement;

            }
            catch (Exception exception)
            {

                throw new DCRSWorkflowException(
                    string.Format("Failed to serialize DCRS specification. Error message: {0}",
                                  DCRSWorkflowException.GetCombinedMessage(exception)));
            }
        }

        public static string SerializeDCRSSpecificationToString(DCRSSpecification specification)
        {

            var specificationElement = SerializeDCRSSpecificationToXElement(specification);

            var stringWriter = new StringWriter();

            specificationElement.Save(stringWriter);

            var specXml = stringWriter.ToString();

            stringWriter.Close();

            return specXml;

        }

        #endregion


        #region 2. Deserialization Methods.

        public static DCRSSpecification DeserializeDCRSSpecificationXml(string dcrsXml)
        {

            if (string.IsNullOrEmpty(dcrsXml)) throw new ArgumentNullException("dcrsXml");

            DCRSSpecification dcrsSpecification;
            try
            {

                var reader = new StringReader(dcrsXml);

                // Here the root could be dcrs-specification or any top level element like dcrs-process
                // So we code assuming that it is not the dcrs-specification element.
                var rootElement = XElement.Load(reader);

                #region 1. Deserialize Modelname and process-id

                try
                {
                    // Extract the model name and process Id.
                    dcrsSpecification = new DCRSSpecification
                    {
                        ModelName =
                            (rootElement.DescendantsAndSelf(
                                GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-specification").
                            Select(specElement => specElement.Attribute("model-name").Value)).Single(),
                        ProcessId = int.Parse(rootElement.DescendantsAndSelf(
                            GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-specification").
                                                            Select(specElement =>
                                                                    specElement.Attribute("process-id").
                                                                        Value).First())
                    };
                }
                catch (Exception exception)
                {
                    throw new DCRSWorkflowException(
        string.Format("Failed to extract model-name or process-id values. Look for specifically invalid data in attributes on <dcrs-specification> section of Xml input. Error message: {0}. ",
                      DCRSWorkflowException.GetCombinedMessage(exception)));
                }

                #endregion

                #region 2. Deserialize Resources.

                try
                {
                    // Deserializing resources.

                    // Roles 
                    foreach (var roleElement in rootElement.Descendants(GlobalDeclarations.DCRS_NAMESPACE_URI + "roles").Elements())
                    {
                        dcrsSpecification.Roles.Add(roleElement.Value);
                    }

                    // Principals 
                    foreach (var roleElement in rootElement.Descendants(GlobalDeclarations.DCRS_NAMESPACE_URI + "principals").Elements())
                    {
                        dcrsSpecification.Principals.Add(roleElement.Value);
                    }


                    // Actions 
                    foreach (var roleElement in rootElement.Descendants(GlobalDeclarations.DCRS_NAMESPACE_URI + "actions").Elements())
                    {
                        dcrsSpecification.ActionList.Add(short.Parse(roleElement.Attribute("action-id").Value),
                                                         roleElement.Value);
                    }

                }
                catch (Exception exception)
                {

                    throw new DCRSWorkflowException(
                        string.Format("Failed to deserialize resources. Look for specifically invalid data in <resources> section of Xml input. Error message: {0}. ",
                                      DCRSWorkflowException.GetCombinedMessage(exception)));

                }

                #endregion


                #region 3. Deserialize Access controls.

                try
                {
                    // Deserializing access-controls.
                    // role-principal-assignments
                    var rolePrincipalAssignments =
                        from ele in
                            rootElement.Descendants(GlobalDeclarations.DCRS_NAMESPACE_URI + "role-principal-assignment")
                            .ToDictionary(
                                roleprincipalAssign => roleprincipalAssign.Attribute("role-name").Value,
                                roleprincipalAssign => roleprincipalAssign.Elements().Select(
                                    roleElement => roleElement.Value).ToList())
                        select ele;

                    foreach (KeyValuePair<string, List<string>> rolePrincipalAssignment in rolePrincipalAssignments)
                        dcrsSpecification.RolesToPrincipalsDictionary.Add(rolePrincipalAssignment.Key,
                                                                          rolePrincipalAssignment.Value);


                    // action-role-assignments

                    var actionRoleAssignemnts =
                        from ele in
                            rootElement.Descendants(GlobalDeclarations.DCRS_NAMESPACE_URI + "action-role-assignment")
                            .ToDictionary(
                                actionRoleAssnEle => short.Parse(actionRoleAssnEle.Attribute("action-id").Value),
                                actionRoleAssnEle => actionRoleAssnEle.Elements().Select(
                                    roleElement => roleElement.Value).ToList())
                        select ele;

                    foreach (var rolePrincipalAssignment in actionRoleAssignemnts)
                        dcrsSpecification.ActionsToRolesDictionary.Add(rolePrincipalAssignment.Key,
                                                                          rolePrincipalAssignment.Value);

                }
                catch (Exception exception)
                {

                    throw new DCRSWorkflowException(
                        string.Format("Failed to deserialize Access controls. Look for specifically invalid data in <access-controls> section of Xml input. Error message: {0}. ",
                                      DCRSWorkflowException.GetCombinedMessage(exception)));

                }

                #endregion


                #region 4. Deserialize constraint-set-collection.

                try
                {
                    // Deserializing constraint-set-collection.
                    // role-principal-assignments

                    dcrsSpecification.Includes = GetContraintsArray(rootElement, "include");

                    dcrsSpecification.Excludes = GetContraintsArray(rootElement, "exclude");

                    dcrsSpecification.Responses = GetContraintsArray(rootElement, "response");

                    dcrsSpecification.Conditions = GetContraintsArray(rootElement, "condition");

                    dcrsSpecification.StrongConditions = GetContraintsArray(rootElement, "strongcondition");

                    dcrsSpecification.Milestones = GetContraintsArray(rootElement, "milestone");

                }
                catch (Exception exception)
                {

                    throw new DCRSWorkflowException(
                        string.Format("Failed to deserialize Access controls. Look for specifically invalid data in <constraint-set-collection> section of Xml input. Error message: {0}. ",
                                      DCRSWorkflowException.GetCombinedMessage(exception)));

                }

                #endregion






            }
            catch (Exception exception)
            {

                throw new DCRSWorkflowException(
    string.Format("Failed to deserialize DCRS specification. Error message: {0}, DCRS Xml:{1}",
                  DCRSWorkflowException.GetCombinedMessage(exception), dcrsXml));

            }




            return dcrsSpecification;
        }

        #endregion







        #region Private Static Methods.
        private static XElement WriteConstrintSet(short[,] constraintset, string type)
        {
            // constraint-set-collection element.
            var constraintsetElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "constraint-set", new XAttribute("type", type));

            for (short index = 0; index < constraintset.GetLength(0); index++)
            {
                var constraintElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "constarint", new XAttribute("source-id", constraintset[index, 0]),
                                                     new XAttribute("target-id", constraintset[index, 1])
                    );

                constraintsetElement.Add(constraintElement);
            }


            return constraintsetElement;
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

                constraintArray[index, 0] = short.Parse(element.Attribute("source-id").Value);

                constraintArray[index, 1] = short.Parse(element.Attribute("target-id").Value);

                index++;

            }

            return constraintArray;



        }

        #endregion


    }


    #region Commented code: LINQ alternatives.




    //dcrsSpecification.ModelName =
    //    (from specElement in
    //         rootElement.DescendantsAndSelf(GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-specification")
    //     let name = specElement.Attribute("model-name").Value
    //     select name).First();


    //            dcrsSpecification.ModelName =
    //(rootElement.DescendantsAndSelf(GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-specification").Select(
    //    specElement => new {specElement, name = specElement.Attribute("model-name").Value}).Select(@t => @t.name)).First();





    #endregion



}