//------------------------------------------------------------------------
// ExtendedLinq: additional operators for LINQ to objects
//
// ExtendedEnumerable.cs
// 
// Igor Ostrovsky, http://www.igoro.com
//------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace IgorO.ExtendedLinq
{
    public static class ExtendedEnumerable
    {
        //
        // Generating sequences
        //
        public static IEnumerable<T> Generate<T>(Func<T> generator) where T : class
        {
            if (generator == null) throw new ArgumentNullException("generator");

            T t;
            while ((t = generator()) != null)
            {
                yield return t;
            }
        }

        public static IEnumerable<T> Generate<T>(Func<Nullable<T>> generator) where T : struct
        {
            if (generator == null) throw new ArgumentNullException("generator");

            Nullable<T> t;
            while ((t = generator()).HasValue)
            {
                yield return t.Value;
            }
        }

        public static IEnumerable<T> FromEnumerator<T>(IEnumerator<T> enumerator)
        {
            if (enumerator == null) throw new ArgumentNullException("enumerator");

            while (enumerator.MoveNext())
            {
                yield return enumerator.Current;
            }
        }

        public static IEnumerable<T> Single<T>(T value)
        {
            return Enumerable.Repeat(value, 1);
        }

        //
        // I/O
        //
        public static IEnumerable<string> ReadLinesFromFile(string path)
        {
            if (path == null) throw new ArgumentNullException("path");
            using (StreamReader file = new StreamReader(path))
            {
                string line;
                while ((line = file.ReadLine()) != null) yield return line;
            }
        }

        public static IEnumerable<string> ReadLinesFromConsole()
        {
            return ReadLinesFrom(Console.In);
        }

        public static IEnumerable<string> ReadLinesFrom(TextReader reader)
        {
            if (reader == null) throw new ArgumentNullException("reader");

            return Generate(() => reader.ReadLine());
        }

        public static void WriteLinesTo<T>(this IEnumerable<T> lines, TextWriter writer)
        {
            if (lines == null) throw new ArgumentNullException("lines");
            if (writer == null) throw new ArgumentNullException("writer");

            lines.ForEach((line) => writer.WriteLine(line.ToString()));
        }

        public static void WriteLinesToConsole<T>(this IEnumerable<T> lines)
        {
            lines.WriteLinesTo(Console.Out);
        }

        public static void WriteLinesToFile<T>(this IEnumerable<T> lines, string path)
        {
            if (path == null) throw new ArgumentNullException("path");

            using (TextWriter file = new StreamWriter(path))
            {
                lines.WriteLinesTo(file);
            }
        }

        //
        // Side effects
        //
        public static IEnumerable<T> Do<T>(this IEnumerable<T> source, Action<T> action)
        {
            if (source == null) throw new ArgumentNullException("source");
            if (action == null) throw new ArgumentNullException("action");

            foreach (T elem in source)
            {
                action(elem);
                yield return elem;
            }
        }

        public static void ForEach<T>(this IEnumerable<T> source, Action<T> action)
        {
            if (source == null) throw new ArgumentNullException("source");
            if (action == null) throw new ArgumentNullException("action");

            foreach (T elem in source)
            {
                action(elem);
            }
        }

        //
        // ToStringPretty
        //
        public static string ToStringPretty<T>(this IEnumerable<T> source)
        {
            return ToStringPretty(source, ",");
        }

        public static string ToStringPretty<T>(this IEnumerable<T> source, string delimiter)
        {
            return ToStringPretty(source, "", delimiter, "");
        }

        public static string ToStringPretty<T>(this IEnumerable<T> source, string before, string delimiter, string after)
        {
            if (source == null) throw new ArgumentNullException("source");

            StringBuilder result = new StringBuilder();
            result.Append(before);

            bool firstElement = true;
            foreach (T elem in source)
            {
                if (firstElement) firstElement = false;
                else result.Append(delimiter);

                result.Append(elem.ToString());
            }

            result.Append(after);
            return result.ToString();
        }

        //
        // Other
        //
        public static IEnumerable<TOut> Combine<TIn1, TIn2, TOut>(
            this IEnumerable<TIn1> in1, IEnumerable<TIn2> in2, Func<TIn1, TIn2, TOut> func)
        {
            if (in1 == null) throw new ArgumentNullException("in1");
            if (in2 == null) throw new ArgumentNullException("in2");
            if (func == null) throw new ArgumentNullException("func");

            using (var e1 = in1.GetEnumerator())
            using (var e2 = in2.GetEnumerator())
            {
                while (e1.MoveNext() && e2.MoveNext())
                {
                    yield return func(e1.Current, e2.Current);
                }
            }
        }

        public static IEnumerable<T> Shuffle<T>(this IEnumerable<T> source)
        {
            return Shuffle(source, new Random());
        }

        public static IEnumerable<T> Shuffle<T>(this IEnumerable<T> source, Random random)
        {
            if (source == null) throw new ArgumentNullException("source");
            if (random == null) throw new ArgumentNullException("random");

            T[] array = source.ToArray();

            for (int i = 0; i < array.Length; i++)
            {
                int r = random.Next(i + 1);
                T tmp = array[r];
                array[r] = array[i];
                array[i] = tmp;
            }

            return array;
        }
    }
}