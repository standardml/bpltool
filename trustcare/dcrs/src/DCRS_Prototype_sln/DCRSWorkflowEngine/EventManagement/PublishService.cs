using System;
using System.Diagnostics;
using System.Reflection;
using System.Threading;

namespace ITU.DK.DCRS.WorkflowEngine.EventManagement
{
    public abstract class PublishService<T> where T : class
    {
        protected static void FireEvent(params object[] args)
        {
            var stackFrame = new StackFrame(1);
            var methodName = stackFrame.GetMethod().Name;

            PublishPersistent(methodName, args);

        }

        public static void FireEventWithMethodname(string methodName, params object[] args)
        {
            PublishPersistent(methodName, args);
        }


        static void PublishPersistent(string methodName, params object[] args)
        {

            T[] subscribers = SubscriptionManager<T>.GetPersistentList(methodName);
            Publish(subscribers, true, methodName, args);

        }


        static void Publish(T[] subscribers, bool closeSubscribers, string methodName, params object[] args)
        {
            WaitCallback fire = delegate(object subscriber)
            {
                Invoke(subscriber as T, methodName, args);
                if (!closeSubscribers) return;
                using (subscriber as IDisposable)
                { }
            };

            Action<T> queueUp = subscriber => ThreadPool.QueueUserWorkItem(fire, subscriber);

            Array.ForEach(subscribers, queueUp);
        }


        static void Invoke(T subscriber, string methodName, object[] args)
        {
            Debug.Assert(subscriber != null);
            Type type = typeof(T);
            var methodInfo = type.GetMethod(methodName);
            try
            {
                methodInfo.Invoke(subscriber, args);
            }
            catch (Exception e)
            {
                Trace.WriteLine(e.Message);
            }
        }
    }
}
