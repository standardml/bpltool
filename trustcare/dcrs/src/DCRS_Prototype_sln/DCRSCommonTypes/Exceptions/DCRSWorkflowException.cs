using System;
using System.Runtime.Serialization;

namespace ITU.DK.DCRS.CommonTypes.Exceptions
{
    /// <summary>
    /// DCRS Application exceptions.
    /// </summary>
    [Serializable]
    public class DCRSWorkflowException : ApplicationException, ISerializable
    {
        const string PREFIX_MESSAGE = "DCRS Workflow Exception: ";

        ///<overloads>Initializes a new instance of the DCRSWorkflowException class.</overloads>
        /// <summary>
        /// Initializes a new instance of the DCRSWorkflowException class.
        /// </summary>
        public DCRSWorkflowException()
        {
        }

        /// <summary>
        /// Initializes a new instance of the DCRSWorkflowException class with supplied error message.
        /// </summary>
        /// <param name="message">Exception message.</param>
        public DCRSWorkflowException(string message)
            : base(message)
        {
        }

        /// <summary>
        /// Initializes a new instance of the DCRSWorkflowException class with supplied 
        /// error message and inner exception.
        /// </summary>
        /// <param name="message">Exception message.</param>
        /// <param name="innerException">Instance of Inner exception.</param>
        public DCRSWorkflowException(string message, Exception innerException)
            : base(message, innerException)
        {
        }

        /// <summary>
        /// Error message explaining exception details. 
        /// </summary>
        /// <remarks>
        /// A standard prefix indicating 
        /// that this was a DCRS Application exception is always added to
        /// the actual message provided in the constructor.
        /// </remarks>
        public override string Message
        {
            get
            {
                if (base.Message.StartsWith(PREFIX_MESSAGE))
                {
                    return base.Message;
                }
                else
                {
                    return PREFIX_MESSAGE + base.Message;
                }
            }
        }

        /// <summary>
        /// Deserialization constructor.
        /// </summary>
        /// <param name="info">Serializaton info.</param>
        /// <param name="context">Streaming context.</param>
        protected DCRSWorkflowException(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
        }

        /// <summary>
        /// Sets a SerializationInfo with all the exception object data targeted for serialization. 
        /// During deserialization, the exception object is reconstituted from the SerializationInfo transmitted over the stream.
        /// </summary>
        /// <param name="serializationInfo">The object that holds the serialized object data.</param>
        /// <param name="serializationContext">The contextual information about the source or destination.</param>
        /// <remarks>
        /// Current implementation does not have any extra fields to serialize.
        /// </remarks>
        void ISerializable.GetObjectData(SerializationInfo serializationInfo, StreamingContext serializationContext)
        {
            base.GetObjectData(serializationInfo, serializationContext);
        }

        /// <summary>
        /// Gets a combined error message from an exception and it's nested inner exceptions, if any.
        /// </summary>
        /// <param name="ex">Exception object.</param>
        /// <returns>Combined error message from an exception and messages from nested inner exceptions if any available.</returns>
        public static string GetCombinedMessage(Exception ex)
        {
            string message = ex.Message;

            Exception innerEx = ex.InnerException;

            int innerExceptionIndex = 0;
            // This is to limit the messages from Inner Exception less tahn 10.
            while (innerEx != null && innerExceptionIndex < 10)
            {
                message += " Inner exception: " + innerEx.Message;

                innerEx = innerEx.InnerException;

                innerExceptionIndex++;
            }

            return message;
        }
    }



}
